;;; test.el --- Tests for phony                      -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; This file is part of phony.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'phony)
(require 'phony-parser)
(require 'phony-talon)
(require 'phony-experimental)

(defmacro phony-test (&rest body)
  "Evaluate BODY in a fresh phony test environment.

In the environment, `phony-mode' is disabled, no rules are bound,
exporters do nothing, and export request debounce is turned off."
  (declare (indent 0))
  `(let ((phony--default-grammar (phony--make-grammar))
         (phony--exported-grammar (phony--make-grammar))
         (phony-export-function #'ignore)
         (phony--dictionary-export-function #'ignore)
         (phony--debounce-export-requests nil)
         (phony-mode-was-enabled phony-mode)
         (phony-interactive-mode-was-enabled phony-interactive-mode)
         (phony-literal-normalizer #'identity))
     (unwind-protect
         (progn
           (phony-mode -1)
           ,@body)
       (phony-interactive-mode
        (if phony-interactive-mode-was-enabled 1 -1))
       ;; The export happening in (phony-mode 1) is fine, as it is
       ;; done still in our test environment.
       (phony-mode (if phony-mode-was-enabled 1 -1)))))

(cl-defstruct (phony-test--diagnostic-specification
               (:constructor phony-test--make-diagnostic-specification)
               (:copier nil))
  "A specification for how diagnostics should be reported."
  ( type 'phony--grammar-error
    :type symbol
    :documentation "The diagnostic type.")
  ( cardinality 1
    :type integer
    :documentation "How many times the diagnostic should appear."))

(defmacro phony-test-should-have-diagnostics (&rest diagnostic-specs)
  "Fail if diagnostics of current grammar are not DIAGNOSTIC-SPECS.

DIAGNOSTIC-SPECS is a list of elements (TYPE . ARGS), where TYPE is the
diagnostic type and ARGS is a plist with the following entries:

  :cardinality          How many times the diagnostic should appear.

If an entry is a symbol, :cardinality defaults to 1.

This macro fails the test if the real diagnostics don't match the
specification exactly."
  `(let ((specs (seq-map
                 (lambda (spec-entry)
                   (when (symbolp spec-entry)
                     (setq spec-entry
                           `(,spec-entry :cardinality 1)))
                   (pcase-let* ((`(,type . ,args) spec-entry))
                     (phony-test--make-diagnostic-specification
                      :type type
                      :cardinality (plist-get args :cardinality))))
                 ',diagnostic-specs))
         (diagnostics (phony--grammar-diagnostics
                       (phony--finalize-grammar))))
     (while-let ((diagnostic (pop diagnostics)))
       (let ((spec (seq-find
                    (lambda (spec) (eq (phony--diagnostic-type diagnostic)
                                       (phony-test--diagnostic-specification-type spec)))
                    specs)))
         (unless spec
           (ert-fail
            (format "Got unexpected diagnostic %s"
                    (phony--diagnostic-type diagnostic))))
         (cl-decf (phony-test--diagnostic-specification-cardinality spec))))
     (dolist (spec specs)
       (let ((cardinality (phony-test--diagnostic-specification-cardinality
                           spec)))
         (when (> cardinality 0)
           (ert-fail
            (format "Too few diagnostics of type %s"
                    (phony-test--diagnostic-specification-type spec))))
         (when (< cardinality 0)
           (ert-fail
            (format "Too many diagnostics of type %s"
                    (phony-test--diagnostic-specification-type spec))))))))

(ert-deftest phony-dictionary-get-setf-roundtrip ()
  "Setting a dictionary entry via setf is readable via the getter."
  (phony-test
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let ((expected 42))
      (setf (phony-dictionary-get "alpha" 'test-dict) expected)
      (should (equal expected (phony-dictionary-get "alpha" 'test-dict))))))

(ert-deftest phony-dictionary-alist-setf-roundtrip ()
  "Setting a dictionary alist via setf is readable via the getter."
  (phony-test
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let ((expected '(("beta" . 2) ("gamma" . 3))))
      (setf (phony-dictionary-alist 'test-dict) expected)
      (should (equal expected (phony-dictionary-alist 'test-dict))))))

(ert-deftest phony-dictionary-definition-triggers-export ()
  "Defining a dictionary only triggers dictionary export."
  (phony-test
    (let* ((dictionary-export-count 0)
           (rule-export-count 0)
           (phony--dictionary-export-function
            (lambda (_grammar) (cl-incf dictionary-export-count)))
           (phony-export-function
            (lambda (_grammar) (cl-incf rule-export-count))))
      (phony-mode 1)
      (phony-define-dictionary test-dict
        '(("alpha" . 1)))
      (should (= dictionary-export-count 1))
      (should (= rule-export-count 0)))))

(ert-deftest phony-open-rule-definition-triggers-export ()
  "Defining an open rule triggers rule export."
  (phony-test
    (let* ((rule-export-count 0)
           (phony-export-function
            (lambda (_grammar) (cl-incf rule-export-count))))
      (phony-mode 1)
      (phony-define-open-rule test-open-rule)
      (should (= rule-export-count 1)))))

(ert-deftest phony-procedure-rule-definition-triggers-export ()
  "Defining a procedure rule triggers rule export."
  (phony-test
    (let* ((rule-export-count 0)
           (phony-export-function
            (lambda (_grammar) (cl-incf rule-export-count))))
      (phony-mode 1)
      (cl-letf (((symbol-function 'server-running-p) #'always))
        (phony-defun test-procedure-rule "alpha"
          t))
      (should (= rule-export-count 1)))))

(ert-deftest phony-dictionary-modification-triggers-export ()
  "Modifying a dictionary only triggers dictionary export."
  (phony-test
    (phony-mode 1)
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let* ((dictionary-export-count 0)
           (rule-export-count 0)
           (phony--dictionary-export-function
            (lambda (_grammar) (cl-incf dictionary-export-count)))
           (phony-export-function
            (lambda (_grammar) (cl-incf rule-export-count))))
      (phony-dictionary-put "beta" 'test-dict 2)
      (should (= dictionary-export-count 1))
      (should (= rule-export-count 0))
      (phony-dictionary-remove "beta" 'test-dict)
      (should (= dictionary-export-count 2))
      (should (= rule-export-count 0))
      (phony-set-dictionary-alist 'test-dict '(("gamma" . 3)))
      (should (= dictionary-export-count 3))
      (should (= rule-export-count 0)))))

(defun phony-test--evaluate (sentence)
  "Return the result of parsing and evaluating SENTENCE.

SENTENCE must parse unambiguously into a single command.

If there is no parse, an error is thrown."
  (let ((result (phony-evaluate sentence 'NO-PARSE)))
    (when (eq result 'NO-PARSE)
      (error "No parse available for '%s'" sentence))
    (car result)))

(ert-deftest phony-pattern-literal-value ()
  "Literal patterns evaluate to themselves."
  (phony-test
    (phony-mode 1)
    (phony-defun return-hello ("why" (hello "hello") "there")
      hello)
    (phony--export-all)
    (should (equal (phony-test--evaluate "why hello there") "hello"))))

(ert-deftest phony-pattern-repetition-value ()
  "Repetition patterns evaluate to a list of the contained pattern."
  (phony-test
    (phony-mode 1)
    (phony-defun repeat-single ((x (+ "z")))
      x)
    (phony-defun repeat-sequence ((x (+ "a" "b")))
      x)
    (phony--export-all)
    (should (equal (phony-test--evaluate "z z z") '("z" "z" "z")))
    (should (equal (phony-test--evaluate "a b a b") '(("a" "b") ("a" "b"))))))

(ert-deftest phony-pattern-sequence-value ()
  "Sequence patterns evaluate to a list of the contained patterns."
  (phony-test
    (phony-mode 1)
    (phony-defun sequence ((x (seq "x" (+ "y"))))
      x)
    (phony--export-all)
    (should (equal (phony-test--evaluate "x y y y") '("x" ("y" "y" "y"))))))

(ert-deftest phony-pattern-optional-value ()
  "Optional patterns evaluate to the contained pattern or nil."
  (phony-test
    (phony-mode 1)
    (phony-defun optional1 ("1" (x (? "x")))
      x)
    (phony-defun optional2 ("2" (x (? "x" "y")))
      x)
    (phony-defun optional3 ("3" (x (? (+ "x"))))
      x)
    (phony--export-all)
    (should (equal (phony-test--evaluate "1 x") "x"))
    (should (equal (phony-test--evaluate "1") nil))
    (should (equal (phony-test--evaluate "2 x y") '("x" "y")))
    (should (equal (phony-test--evaluate "2") nil))
    (should (equal (phony-test--evaluate "3 x x x x") '("x" "x" "x" "x")))
    (should (equal (phony-test--evaluate "3") nil))))

(ert-deftest phony-pattern-alternative-value ()
  "Alternation patterns evaluate to the matched alternative."
  (phony-test
    (phony-mode 1)
    (phony-defun alt ((x (| "x" "y" "z")))
      x)
    (phony--export-all)
    (should (equal (phony-test--evaluate "x") "x"))
    (should (equal (phony-test--evaluate "y") "y"))
    (should (equal (phony-test--evaluate "z") "z"))))

(ert-deftest phony-pattern-binding-in-alternative ()
  "Binding in non-matched alternative gets bound to nil."
  (phony-test
    (phony-mode 1)
    (phony-defun alt ((| "x" (x "y")))
      x)
    (phony--export-all)
    (should (equal (phony-test--evaluate "x") nil))
    (should (equal (phony-test--evaluate "y") "y"))))

(ert-deftest phony-dictionary-modification-parses ()
  "Changing a dictionary reflects immediately in the parser."
  (phony-test
    (phony-mode 1)
    (phony-define-dictionary lol
      '(("lol" . 1337)))
    (phony-defun somelol (lol)
      lol)
    (phony--export-all)
    (should (= (phony-test--evaluate "lol") 1337))
    (should-error (phony-test--evaluate "lmao"))
    (setf (phony-dictionary-get "lmao" 'lol) 42)
    (should (= (phony-test--evaluate "lol") 1337))
    (should (= (phony-test--evaluate "lmao") 42))))

(ert-deftest phony-dictionary-removal ()
  "Removing an element from a dictionary reflects in the parser."
  (phony-test
    (phony-mode 1)
    (phony-define-dictionary lol
      '(("lol" . 1337)))
    (phony-defun somelol (lol)
      lol)
    (phony--export-all)
    (should (= (phony-test--evaluate "lol") 1337))
    (phony-dictionary-remove "lol" 'lol)
    (should-error (phony-test--evaluate "lol"))))

(ert-deftest phony-dictionary-replacement ()
  "Replacing contents of a dictionary reflects in the parser."
  (phony-test
    (phony-mode 1)
    (phony-define-dictionary lol
      '(("lol" . 1337)))
    (phony-defun somelol (lol)
      lol)
    (phony--export-all)
    (should (= (phony-test--evaluate "lol") 1337))
    (should-error (phony-test--evaluate "lmao"))
    (phony-set-dictionary-alist 'lol '(("lmao" . 42)))
    (should-error (phony-test--evaluate "lol"))
    (should (= (phony-test--evaluate "lmao") 42))))

(ert-deftest phony-multi-word-literal-in-procedure-rule ()
  "Parser handles multi-word literals in `phony-defun'."
  (phony-test
    (phony-mode 1)
    (phony-defun multi-word ("one two three")
      123)
    (phony-defun multi-word-wild-whitespace ("  wild\nwhite\t\t   space")
      42)
    (phony--export-all)
    (should (= (phony-test--evaluate "one two three") 123))
    (should (= (phony-test--evaluate "wild white space") 42))))

(ert-deftest phony-multi-word-literal-in-dictionary ()
  "Parser handles multi-word literals in `phony-define-dictionary'."
  (phony-test
    (phony-mode 1)
    (phony-define-dictionary some-dictionary
      '(("one" . 1)
        ("one two" . 2)
        ("one two three" . 3)
        ("  wild\nwhite\t\t   space" . 42)))
    (phony-defun some-rule ((v some-dictionary))
      v)
    (phony--export-all)
    (should (= (phony-test--evaluate "one") 1))
    (should (= (phony-test--evaluate "one two") 2))
    (should (= (phony-test--evaluate "one two three") 3))
    (should (= (phony-test--evaluate "wild white space") 42))))

(ert-deftest phony-undefined-rule-in-pattern ()
  "Undefined rules in patterns yield diagnostics."
  (phony-test
    (phony-defun some-rule ("hah" (x undefined-rule))
      x)
    (phony-test-should-have-diagnostics phony--undefined-rule)))

(ert-deftest phony-unreachable-undefined-rule-in-pattern ()
  "Undefined rules in unreachable patterns do not yield diagnostics."
  (phony-test
    (phony-defun some-rule ("hah" (x undefined-rule))
      :interactive nil
      x)
    (phony-test-should-have-diagnostics)))

(ert-deftest phony-undefined-rule-in-alternatives ()
  "Undefined rules in open rule alternatives yield diagnostics."
  (phony-test
    (phony-define-open-rule open-rule
      :alternatives (undefined-rule))
    (phony-defun some-rule ("hah" (x open-rule))
      x)
    (phony-test-should-have-diagnostics phony--undefined-rule)))

(ert-deftest phony-unreachable-undefined-rule-in-alternatives ()
  "Undefined rules in unreachable alternatives do not yield diagnostics."
  (phony-test
    (phony-define-open-rule open-rule
      :alternatives (undefined-rule))
    (phony-defun some-rule ("hah")
      5)
    (phony-test-should-have-diagnostics)))

(ert-deftest phony-multiple-undefined-rule-diagnostics ()
  "References to different undefined rules gets a diagnostic each."
  (phony-test
    (phony-defun some-rule ("hah" (x undefined-rule))
      x)
    (phony-defun some-other-rule ("hah" (x other-undefined-rule))
      x)
    (phony-test-should-have-diagnostics
     (phony--undefined-rule
      :cardinality 2))))

(ert-deftest phony-undefined-rule-diagnostic-deduplication-one-rule ()
  "Multiple references to undefined rule in one rule gets deduplicated."
  (phony-test
    (phony-defun some-rule ("hah" (x undefined-rule) (y undefined-rule))
      (cons x y))
    (phony-test-should-have-diagnostics
     (phony--undefined-rule
      :cardinality 1))))

(ert-deftest phony-undefined-rule-diagnostic-deduplication ()
  "Multiple references to same undefined rule gets deduplicated."
  (ert-skip "Not implemented")
  (phony-test
    (phony-defun some-rule ("hah" (x undefined-rule))
      x)
    (phony-defun some-other-rule ("hah" (x undefined-rule))
      x)
    (phony-test-should-have-diagnostics
     (phony--undefined-rule
      :cardinality 1))))

(ert-deftest phony-open-rule-contribution ()
  "Rule contributions work."
  (phony-test
    (phony-define-open-rule number)
    (phony-defun three "three"
      :contributes-to number
      3)
    (phony-defun five "five"
      :contributes-to number
      5)
    (phony-defun add ("add" (first number) "and" (second number))
      (+ first second))
    (phony--export-all)
    (should (= (phony-test--evaluate "add five and three") 8))
    (should (= (phony-test--evaluate "add five and five") 10))
    (should (= (phony-test--evaluate "add three and three") 6))))

(ert-deftest phony-contribution-to-undefined-rule ()
  "Contribution to undefined rule yields diagnostic."
  (phony-test
    (phony-defun three "three"
      :contributes-to number
      3)
    (phony-defun add ("add" (first three) "and" (second three))
      (+ first second))

    (phony-test-should-have-diagnostics phony--undefined-rule)))

(ert-deftest phony-contribution-to-non-open-rule ()
  "Contribution to non-open rule yields diagnostic."
  (phony-test
    (phony-defun number "five" 5)
    (phony-defun three "three"
      :contributes-to number
      3)
    (phony-defun add ("add" (first number) "and" (second number))
      (+ first second))

    (phony-test-should-have-diagnostics phony--contribution-to-non-open-rule)))

(ert-deftest phony-open-rule-contribution-to-module ()
  "Rule contributions work."
  (phony-test
    (phony-define-module module)
    (phony-define-open-rule number
      :module module)
    (phony-defun three "three"
      :contributes-to number
      3)
    (phony-defun five "five"
      :contributes-to number
      5)
    (phony-defun add ("add" (first number) "and" (second number))
      (+ first second))
    (phony--export-all)
    (should (= (phony-test--evaluate "add five and three") 8))
    (should (= (phony-test--evaluate "add five and five") 10))
    (should (= (phony-test--evaluate "add three and three") 6))))

(ert-deftest phony-open-rule-alternatives ()
  "Given alternatives work."
  (phony-test
    (phony-define-open-rule number
      :alternatives (three))
    (phony-defun three "three"
      3)
    (phony-defun five "five"
      :contributes-to number
      5)
    (phony-defun add ("add" (first number) "and" (second number))
      (+ first second))
    (phony--export-all)
    (should (= (phony-test--evaluate "add five and three") 8))
    (should (= (phony-test--evaluate "add five and five") 10))
    (should (= (phony-test--evaluate "add three and three") 6))))

(ert-deftest phony-interface-uses-provider ()
  "Providers are used in place of interfaces."
  (phony-test
    (phony-define-interface number)
    (phony-defun my-number "three"
      :provides number
      3)
    (phony-defun double ("double" number)
      (* 2 number))
    (phony--export-all)
    (should (= (phony-test--evaluate "double three") 6))))

(ert-deftest phony-interface-honors-mode ()
  "Interfaces honor the mode parameter."
  (phony-test
    (phony-define-interface number
      :mode text-mode)
    (phony-defun my-number "three"
      :provides number
      3)
    (phony-defun double ("double" number)
      (* 2 number))
    (phony--export-all)
    (should-error (phony-test--evaluate "double three"))
    (text-mode)
    (should (= (phony-test--evaluate "double three") 6))))

(ert-deftest phony-nestled-interface-honors-mode ()
  "Nestled interfaces honor the mode parameter."
  (phony-test
    (phony-define-interface number
      :mode (fundamental-mode text-mode))
    (phony-define-interface number-indirection
      :mode (fundamental-mode prog-mode)
      :provides number)
    (phony-defun my-number "three"
      :provides number-indirection
      3)
    (phony-defun double ("double" number)
      (* 2 number))
    (phony--export-all)
    (prog-mode)
    (should-error (phony-test--evaluate "double three"))
    (text-mode)
    (should-error (phony-test--evaluate "double three"))
    (fundamental-mode)
    (should (= (phony-test--evaluate "double three") 6))))

(ert-deftest phony-interface-nestled ()
  "Nestled interfaces resolve."
  (phony-test
   (phony-define-interface number)
   (phony-define-interface real
     :provides number)
   (phony-define-dictionary natural
     :provides real
     '(("elite" . 1337)))
   (phony-defun double ("double" number)
     (* 2 number))
   (phony--export-all)
   (should (= (phony-test--evaluate "double elite") 2674))))

(ert-deftest phony-interface-nestled-without-provider ()
  "Nestled interfaces without a provider yield a diagnostic."
  (phony-test
    (phony-define-interface number)
    (phony-define-interface real
      :provides number)
    (phony-defun double ("double" number)
      (* 2 number))

    (phony-test-should-have-diagnostics phony--provider-not-found)))

(ert-deftest phony-interface-cycle-detection ()
  "Cyclic interfaces error."
  (phony-test
    (phony-define-interface number
      :provides number)
    (phony-defun double ("double" number)
      (* 2 number))

    (phony-test-should-have-diagnostics phony--interface-cycle)))

(ert-deftest phony-interface-default-cycle-detection ()
  "Cyclic interface defaults error."
  (phony-test
    (phony-define-interface number
      :default number)
    (phony-defun double ("double" number)
      (* 2 number))

    (phony-test-should-have-diagnostics phony--interface-cycle)))

(ert-deftest phony-interface-undefined-default ()
  "Undefined defaults error."
  (phony-test
    (phony-define-interface number
      :default some-number)
    (phony-defun double ("double" number)
      (* 2 number))

    (phony-test-should-have-diagnostics phony--undefined-interface-default)))

(ert-deftest phony-interface-undefined-default-unreachable ()
  "Undefined defaults do not error when interface is unreachable."
  (phony-test
    (phony-define-interface number
      :default some-number)
    (phony-defun double ("double two")
      (* 2 2))

    (phony-test-should-have-diagnostics)))

(ert-deftest phony-interface-undefined-default-unsued ()
  "Undefined defaults do not error when unused."
  (phony-test
    (phony-define-interface number
      :default some-number)
    (phony-defun three "three"
      :provides number
      3)
    (phony-defun double ("double" number)
      (* 2 number))

    (phony-test-should-have-diagnostics)))

(ert-deftest phony-interface-uses-selected-provider ()
  "Only the selected provider is used for interface."
  (phony-test
    (phony-define-module number-module)
    (phony-define-interface number
      :module number-module)
    (phony-define-module three-module)
    (phony-define-module five-module)

    (phony-defun three "three"
      :module three-module
      :provides number
      3)
    (phony-defun five "five"
      :module five-module
      :provides number
      5)
    (phony-defun double ("double" number)
      (* 2 number))

    (phony-provide-from number-module three-module)
    (phony--export-all)
    (should (= (phony-test--evaluate "double three") 6))
    (should-error (phony-test--evaluate "double five"))

    (phony-provide-from number-module five-module)
    (phony--export-all)
    (should-error (phony-test--evaluate "double three"))
    (should (= (phony-test--evaluate "double five") 10))))

(ert-deftest phony-interface-selected-vs-user-provider ()
  "User providers are preferred over module providers."
  (phony-test
   (phony-define-module number-module)
   (phony-define-module some-module)
   (phony-define-interface number
     :module number-module)
   (phony-defun my-number "three"
     :provides number
     3)
   (phony-defun module-number "five"
     :provides number
     :module some-module)
   (phony-defun double ("double" number)
     (* 2 number))

   (phony-provide-from number-module some-module)
   (phony--export-all)
   (should (= (phony-test--evaluate "double three") 6))))

(ert-deftest phony-interface-error-on-multiple-providers ()
  "Interfaces can't resolve on multiple providers."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module my-numbers)

   (phony-defun three "three"
     :module my-numbers
     :provides number
     3)
   (phony-defun five "five"
     :module my-numbers
     :provides number
     5)
   (phony-defun double ("double" number)
     (* 2 number))

   (phony-provide-from number-module my-numbers)

   (phony-test-should-have-diagnostics phony--ambiguous-interface)))

(ert-deftest phony-interface-error-on-no-selected-provider ()
  "Reachable interfaces must have a selected provider."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module three-module)
   (phony-define-module five-module)

   (phony-defun three "three"
     :module three-module
     :provides number
     3)
   (phony-defun five "five"
     :module five-module
     :provides number
     5)
   (phony-defun double ("double" number)
     (* 2 number))

   (phony-test-should-have-diagnostics phony--provider-not-found)))

(ert-deftest phony-interface-elide-unused ()
  "Interfaces with more or less than one provider is ok if unreachable."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module my-numbers)

   (phony-defun three "three"
     :module my-numbers
     :provides number
     3)
   (phony-defun five "five"
     :module my-numbers
     :provides number
     5)
   (phony-defun double ("double" "four")
     (* 2 4))

   (phony--export-all)
   (should (= (phony-test--evaluate "double four") 8))

   (phony-provide-from number-module my-numbers)
   (phony--export-all)
   (should (= (phony-test--evaluate "double four") 8))))

(ert-deftest phony-interface-multiple-consumptions ()
  "Interfaces may be consumed multiple times."
  (phony-test
   (phony-define-interface number)

   (phony-defun three "three"
     :provides number
     3)
   (phony-defun sum ("add" (x number) "and" (y number))
     (+ x y))

   (phony--export-all)
   (should (= (phony-test--evaluate "add three and three") 6))))

(ert-deftest phony-interface-default-provider ()
  "Interface default providers are picked by default."
  (phony-test
   (phony-define-module interface-module)
   (phony-define-module provider-module)
   (phony-define-interface number
     :default three
     :module interface-module)

   (phony-defun three "three"
     :provides number
     :module provider-module
     3)
   (phony-defun five "five"
     :provides number
     :module provider-module
     5)

   (phony-defun double ("double" number)
     (* 2 number))

   (phony--export-all)
   (should (= (phony-test--evaluate "double three") 6))
   (should-error (phony-test--evaluate "double five"))))

(ert-deftest phony-interface-default-provider-vs-selection ()
  "Interface default providers are lower priority than user selections."
  (phony-test
   (phony-define-module interface-module)
   (phony-define-module provider-module)
   (phony-define-interface number
     :default three
     :module interface-module)

   (phony-defun three "three"
     :provides number
     :module interface-module
     3)
   (phony-defun five "five"
     :provides number
     :module provider-module
     5)

   (phony-defun sum ("double" number)
     (* 2 number))

   (phony-provide-from interface-module provider-module)
   (phony--export-all)
   (should (= (phony-test--evaluate "double five") 10))
   (should-error (phony-test--evaluate "double three"))))

(ert-deftest phony-interface-default-provider-vs-user-provider ()
  "Interface default providers are lower priority than user providers."
  (phony-test
    (phony-define-module interface-module)
    (phony-define-interface number
      :default three
      :module interface-module)

    (phony-defun three "three"
      :provides number
      :module interface-module
      3)
    (phony-defun five "five"
      :provides number
      5)

    (phony-defun sum ("double" number)
      (* 2 number))

    (phony--export-all)
    (should (= (phony-test--evaluate "double five") 10))
    (should-error (phony-test--evaluate "double three"))))

(ert-deftest phony-left-recursion-diagnostic ()
  "Rules referring to themselves before matching a word generate diagnostics."
  (phony-test
    (phony-defun some-word ((word another-word))
      word)
    (phony-defun another-word ((word some-word))
      word)
    (phony-test-should-have-diagnostics phony--left-recursion)))

(ert-deftest phony-left-recursion-diagnostic-through-optional ()
  "Left recursion after patterns that can match nothing is diagnosed."
  (phony-test
    (phony-define-open-rule things)
    (phony-defun more-things ((? "maybe") (* "many") things "end")
      :contributes-to things
      things)
    (phony-test-should-have-diagnostics phony--left-recursion)))

(ert-deftest phony-recursive-rule ()
  "A rule can refer to itself after matching a word."
  (phony-test
    (phony-defun nesting ((| (seq "open" (inner nesting) "close")
                             (leaf "leaf")))
      (if leaf 0 (1+ inner)))
    (phony-test-should-have-diagnostics)
    (phony--export-all)
    (should (= (phony-test--evaluate "leaf") 0))
    (should (= (phony-test--evaluate "open open leaf close close") 2))
    (should-error (phony-test--evaluate "open leaf close close"))))

(ert-deftest phony-mutually-recursive-rules ()
  "Rules can refer to each other, also through open rules."
  (phony-test
    (phony-define-open-rule expression)
    (phony-defun small-number ((n (| "one" "two")))
      :contributes-to expression
      (if (equal n "one") 1 2))
    (phony-defun sum ("sum of" (a expression) "and" (b expression))
      :contributes-to expression
      (+ a b))
    (phony-defun compute ("compute" expression)
      expression)
    (phony-test-should-have-diagnostics)
    (phony--export-all)
    (should (= (phony-test--evaluate "compute two") 2))
    (should (= (phony-test--evaluate "compute sum of one and sum of two and two")
               5))))

(ert-deftest phony-builtin-word ()
  "Word builtins parse."
  ;; TODO: Check against phony-stdlib-word instead
  (phony-test
    (phony-defun some-word ((word phony--builtin-word))
      word)
    (phony--export-all)
    (should (equal (phony-test--evaluate "cwambabala")
                   "cwambabala"))))

(ert-deftest phony-builtin-phrase ()
  "Phrase builtins parse."
  ;; TODO: Check against phony-stdlib-phrase instead.
  (phony-test
    (phony-defun some-phrase ((phrase phony--builtin-phrase))
      phrase)
    (phony--export-all)
    (should (equal (phony-test--evaluate "abra ha da bruh")
                   "abra ha da bruh"))))

(ert-deftest phony-mode-deactivation ()
  "Rules can be enabled with according to mode."
  (phony-test
    (phony-defun show-phrase ((phrase phony--builtin-phrase))
      :mode text-mode
      phrase)
    (phony--export-all)
    (should-error (phony-test--evaluate "abra ha da bruh"))
    (with-temp-buffer
      (text-mode)
      (should (equal (phony-test--evaluate "abra ha da bruh")
                     "abra ha da bruh")))))

(ert-deftest phony-disabled-rules-no-contribute-externally ()
  "Disabled rules do not contribute to open rules outside module."
  (phony-test
   (phony-define-module x)
   (phony-defun x "x"
     :module x
     :contributes-to z
     5)
   (phony-define-open-rule z)
   (phony-defun y ("y" z)
     (+ z 3))
   (phony-enable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))
   (phony-disable-module 'x)
   (phony--export-all)
   (should-error (phony-test--evaluate "y x"))))

(ert-deftest phony-disabled-rules-contribute-internally ()
  "Disabled rules do contribute to open rules inside module."
  (phony-test
   (phony-define-module x)
   (phony-defun x "x"
     :module x
     :contributes-to z
     5)
   (phony-define-open-rule z
     :module x)
   (phony-defun y ("y" z)
     (+ z 3))
   (phony-enable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))
   (phony-disable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))))

(ert-deftest phony-disabled-rules-may-be-used-explicitly ()
  "Disabled rules can still be used explicitly."
  (phony-test
    (phony-define-module x)
    (phony-defun x "x"
      :module x
      5)
    (phony-defun y ("y" x)
      (+ x 3))
    (phony-disable-module 'x)
    (phony--export-all)
    (should (equal (phony-test--evaluate "y x") 8))))

(ert-deftest phony-disabled-rules-not-interactive ()
  "Disabled rules are not interactive."
  (phony-test
   (phony-define-module x)
   (phony-defun x "x"
     :interactive t
     :module x
     5)
   (phony-disable-module 'x)
   (phony--export-all)
   (should-error (phony-test--evaluate "x"))
   (phony-enable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "x") 5))))

(ert-deftest phony-conditional-interactivity ()
  "Procedure rules can be interactive conditional on active modes."
  (phony-test
   (phony-defun almost-always "always"
     :interactive t
     t)
   (phony-defun text "text"
     :interactive text-mode
     t)
   (phony--export-all)
   (should-error (phony-test--evaluate "text"))
   (should (phony-test--evaluate "always"))
   (text-mode)
   (phony-interactive-mode -1)
   (should (phony-test--evaluate "text"))
   (should-error (phony-test--evaluate "always"))))

(ert-deftest phony-conditional-interactivity-multiple-modes ()
  "Procedure rules can be interactive conditional on active modes."
  (phony-test
   (phony-defun t-and-text "test"
     :interactive (text-mode t)
     t)
   (phony--export-all)
   (should (phony-test--evaluate "test"))
   (phony-interactive-mode -1)
   (should-error (phony-test--evaluate "test"))
   (text-mode)
   (should (phony-test--evaluate "test"))))

(ert-deftest phony-anchors ()
  "Anchors limit parses correctly."
  (phony-test
   (phony-defun x "x"
     :anchor-beginning t
     t)
   (phony-defun y "y"
     :anchor-end t
     t)
   (phony-defun z "z"
     :anchor-end t
     :anchor-beginning t
     t)
   (phony-defun w "w"
     t)
   (phony--export-all)
   (should (phony-test--evaluate "x y"))
   (should (phony-test--evaluate "x w w w y"))
   (should (phony-test--evaluate "z"))

   (should-error (phony-test--evaluate "y w"))
   (should-error (phony-test--evaluate "w x"))
   (should-error (phony-test--evaluate "z w"))
   (should-error (phony-test--evaluate "w z"))))

(ert-deftest phony-literal-normalizer-binding ()
  "Binding `phony-literal-normalizer' normalizes literals and dictionaries."
  (phony-test
    (phony-define-dictionary animal
      '(("cat" . cat) ("Big Dog" . dog)))
    (phony-defun pet ("Pet the" animal)
      animal)
    (phony--export-all)
    (should-error (phony-test--evaluate "pet the CAT"))
    (should (eq (phony-test--evaluate "Pet the Big Dog") 'dog))
    (let ((phony-literal-normalizer #'downcase))
      (should (eq (phony-test--evaluate "pet the CAT") 'cat))
      (should (eq (phony-test--evaluate "PET THE big dog") 'dog)))
    (should-error (phony-test--evaluate "pet the big dog"))
    (should (eq (phony-test--evaluate "Pet the Big Dog") 'dog))))

(ert-deftest phony-parser-literal-normalizer ()
  "`phony-literal-normalizer' works as advertised."
  (phony-test
    (setq phony-literal-normalizer #'length)
    (phony-defun x ("1234" (? "12") "123")
      42)
    (phony--export-all)
    (should (phony-test--evaluate "abcd abc"))
    (should (phony-test--evaluate "abcd ab abc"))

    (should-error (phony-test--evaluate "abc abcd"))
    (should-error (phony-test--evaluate "abcd a abc"))))

(ert-deftest phony-talon-export ()
  "The talon exporter exports with correct syntax."
  (unless (executable-find "python")
    (let ((message "Python is not installed in the environment"))
      (if noninteractive
          (error "%s" message)
        (ert-skip message))))
  (phony-test
    (let* ((directory (make-temp-file "phony-talon-test" t))
           (phony-output-directory directory))
      (unwind-protect
          (progn
            (phony-define-module some-module)
            (phony-enable-module 'some-module)
            (phony-define-open-rule number)
            (phony-defun three "three"
              :contributes-to number
              :module some-module
              3)
            (phony-define-interface additional-number
              :contributes-to number)
            (phony-defun four "four"
              :provides additional-number)
            (phony-defun five "five"
              :contributes-to number
              3)
            (phony-define-dictionary more-numbers
              :contributes-to number
              '(("pi" . 3.14) ("elite" . 1337)))
            (phony-defun yet-more-numbers ((x (| "i" "neggy")))
              :contributes-to number
              (pcase x ("i" (sqrt -1)) ("neggy" -1)))
            (phony-defun double ("double" number)
              (* 2 number))
            (phony-defun insert-word ("insert" (word phony--builtin-word))
              (insert word))
            (phony-defun insert-phrase ("insert" (phrase phony--builtin-phrase))
              (insert phrase))
            (phony--export-all)
            (phony-talon--generate-configuration phony--exported-grammar)
            (with-temp-buffer
              (unless (zerop (call-process "python" nil t nil "-m" "py_compile"
                                           (phony-talon--output-path "rules.py")
                                           (phony-talon--output-path "evaluate_elisp.py")
                                           (phony-talon--output-path "read_dictionaries.py")))
                (error "Syntax error in python: %s" (buffer-string)))))
        (delete-directory directory t)))))

(ert-deftest phony-stdlib-number-parsing ()
  "Numbers parse correctly."
  ;; TODO: Test against utterances instead of number sequences
  (should (equal (phony-stdlib--parse-number '(100 5 1000)) 105000))
  (should (equal (phony-stdlib--parse-number '(1 1000000 5 100 1 1000)) 1501000))
  (should (equal (phony-stdlib--parse-number '(1 1000000 5 100 1 1000 1 100 6)) 1501106))
  (should (equal (phony-stdlib--parse-number '(1 2 3)) 123))
  (should (equal (phony-stdlib--parse-number '(1 23)) 123))
  (should (equal (phony-stdlib--parse-number '(10 4)) 104))
  (should (equal (phony-stdlib--parse-number '(10 60 6)) 1066))
  (should (equal (phony-stdlib--parse-number '(19 0 6)) 1906))
  (should (equal (phony-stdlib--parse-number '(20 0 1)) 2001))
  (should (equal (phony-stdlib--parse-number '(20 20)) 2020))
  (should (equal (phony-stdlib--parse-number '(1 1000 1)) 1001))
  (should (equal (phony-stdlib--parse-number '(1 1000 10)) 1010))
  (should (equal (phony-stdlib--parse-number '(1 100 20 3 1000 4 100 50 6)) 123456))
  (should (equal (phony-stdlib--parse-number '(1 20 3 1000 4 50 6)) 123456)))

;;;; Parsing without a recognition engine

(defmacro phony-test-collecting-warnings (variable &rest body)
  "Evaluate BODY with the messages of warnings collected in VARIABLE."
  (declare (indent 1))
  `(let ((,variable '()))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (_type message &rest _)
                  (push message ,variable))))
       ,@body)))

(defun phony-test--parses-p (utterance)
  "Return non-nil if UTTERANCE parses with `phony-grammar'."
  (phony-parser--parse-utterance utterance (phony-grammar)))

(ert-deftest phony-grammar-without-phony-mode ()
  "`phony-grammar' is up to date without `phony-mode'."
  (phony-test
    (phony-defun hello "hello"
      'hello)
    (should (phony-test--parses-p "hello"))
    (should-not (phony-test--parses-p "goodbye"))
    (let ((grammar (phony-grammar)))
      (should (eq grammar (phony-grammar)))
      (phony-defun goodbye "goodbye"
        'goodbye)
      (should-not (eq grammar (phony-grammar))))
    (should (phony-test--parses-p "goodbye"))
    (phony-remove-rule 'goodbye)
    (should-not (phony-test--parses-p "goodbye"))))

(ert-deftest phony-grammar-tracks-modules ()
  "`phony-grammar' is prepared again when a module is enabled or disabled."
  (phony-test
    (cl-letf (((symbol-function 'message) #'ignore))
      (phony-define-module some-module)
      (phony-defun module-rule "module rule"
        :module some-module
        t)
      (should-not (phony-test--parses-p "module rule"))
      (phony-enable-module 'some-module)
      (should (phony-test--parses-p "module rule"))
      (phony-disable-module 'some-module)
      (should-not (phony-test--parses-p "module rule")))))

(ert-deftest phony-grammar-with-errors ()
  "`phony-grammar' warns about errors and keeps the last grammar without."
  (phony-test
    (phony-test-collecting-warnings warnings
      (phony-defun broken ("broken" undefined-rule)
        undefined-rule)
      (should (phony-grammar))
      (should-not (phony-test--parses-p "broken"))
      (should (seq-some (lambda (warning) (string-search "undefined-rule" warning))
                        warnings))
      (phony-remove-rule 'broken)
      (phony-defun fine "fine"
        t)
      (should (phony-test--parses-p "fine"))
      (setq warnings '())
      (phony-defun broken ("broken" undefined-rule)
        undefined-rule)
      (should (phony-test--parses-p "fine"))
      (should-not (phony-test--parses-p "broken"))
      (should (= (length warnings) 1))
      ;; Warnings aren't repeated while nothing changes
      (phony-grammar)
      (should (= (length warnings) 1)))))

(ert-deftest phony-parse-all-parses ()
  "`phony-parse' returns every parse, the fewest free-text words first."
  (phony-test
    (phony-defun say ("say" (words phony--builtin-phrase))
      words)
    (phony-defun stop "stop"
      'stop)
    (should (equal (phony-parse "say hello stop")
                   '(((say "hello") (stop))
                     ((say "hello stop")))))
    (should (equal (phony-parse '("stop" "stop"))
                   '(((stop) (stop)))))
    (should (equal (mapcar (lambda (parse)
                             (mapcar #'phony-evaluate-ast parse))
                           (phony-parse "say hello stop"))
                   '(("hello" stop) ("hello stop"))))))

(ert-deftest phony-parse-rule ()
  "`phony-parse-rule' parses any rule."
  (phony-test
    (phony-define-dictionary colors
      '(("red" . red) ("dark red" . dark-red)))
    (phony-defun shade ((? "very") colors)
      :interactive nil
      colors)
    (should (equal (mapcar #'phony-evaluate-ast (phony-parse-rule 'shade "very dark red"))
                   '(dark-red)))
    (should (equal (mapcar #'phony-evaluate-ast (phony-parse-rule 'colors "red"))
                   '(red)))
    (should-not (phony-parse-rule 'shade "blue"))
    (should-error (phony-parse-rule 'undefined "red"))))

(ert-deftest phony-parse-spans ()
  "`phony-parse-spans' returns each command at each position."
  (phony-test
    (phony-defun say ("say" (words phony--builtin-phrase))
      words)
    (phony-defun stop "stop"
      'stop)
    (phony-defun first "first"
      :anchor-beginning t
      'first)
    (phony-defun last "last"
      :anchor-end t
      'last)
    (should (equal (phony-parse-spans "say hello stop")
                   [((2 (say "hello") 1) (3 (say "hello stop") 2))
                    ()
                    ((3 (stop) 0))]))
    (should (equal (phony-parse-spans "first first last last")
                   [((1 (first) 0)) () () ((4 (last) 0))]))
    (should (equal (phony-parse-spans '()) []))))

(ert-deftest phony-parse-spans-removes-duplicates ()
  "`phony-parse-spans' returns each parse once."
  (phony-test
    (phony-defun greet ((? "hello") (? "hello") "there")
      t)
    (should (equal (phony-parse-spans "hello there")
                   [((2 (greet) 0)) ((2 (greet) 0))]))))

(ert-deftest phony-interactive-rules ()
  "`phony-interactive-rules' lists the rules that can be said now."
  (phony-test
    (phony-defun always "always"
      t)
    (phony-defun in-text "in text"
      :interactive text-mode
      t)
    (phony-defun only-in-text "only in text"
      :mode text-mode
      t)
    (phony-defun part "part"
      :interactive nil
      t)
    (with-temp-buffer
      (should (equal (phony-interactive-rules) '(always)))
      (text-mode)
      (should (equal (phony-interactive-rules) '(always in-text only-in-text))))))

(ert-deftest phony-rule-introspection ()
  "Rules can be inspected through the public accessors."
  (phony-test
    (phony-define-dictionary colors
      "Some colors."
      '(("red" . red)))
    (phony-defun paint ("paint" (? "it") (how (| "dark" "light")) colors (+ "please"))
      "Paint it in COLORS.
More text."
      (list how colors))
    (should (equal (phony-rule-documentation 'paint) "Paint it in COLORS.\nMore text."))
    (should (equal (phony-rule-documentation 'colors) "Some colors."))
    (should (equal (phony-rule-pattern 'paint)
                   '(seq "paint" (\? "it") (how (| "dark" "light")) (colors colors)
                         (+ "please"))))
    (should (equal (funcall (phony-rule-function 'paint) "dark" 'red)
                   '("dark" red)))
    (should-error (phony-rule-pattern 'colors))
    (should-error (phony-rule-documentation 'undefined))))

(ert-deftest phony-interface-provider ()
  "`phony-interface-provider' resolves interfaces."
  (phony-test
    (phony-define-interface outer)
    (phony-define-interface inner
      :provides outer)
    (phony-defun provider "provide"
      :provides inner
      t)
    (phony-define-interface unprovided)
    (should (eq (phony-interface-provider 'outer) 'provider))
    (should (eq (phony-interface-provider 'inner) 'provider))
    (should-error (phony-interface-provider 'provider))))

(ert-deftest phony-pattern-arguments ()
  "`phony-pattern-arguments' gives the arguments as `phony-defun' does."
  (should (equal (phony-pattern-arguments "hello") '()))
  (should (equal (phony-pattern-arguments '("go" (? (count number)) (| left right)))
                 '(count left right)))
  (should (equal (phony-pattern-arguments '((words (+ word)) (_ignored "x")))
                 '(words _ignored))))

;;; test.el ends here
