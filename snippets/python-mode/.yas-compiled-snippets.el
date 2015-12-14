;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("ent" "def __enter__(self):\n    $0\n\n    return self" "__enter__" nil nil nil nil nil nil)
                       ("ex" "def __exit__(self, type, value, traceback):\n    $0" "__exit__" nil nil nil nil nil nil)
                       ("getit" "def __getitem__(self, ${1:key}):\n    $0" "__getitem__" nil nil nil nil nil nil)
                       ("len" "def __len__(self):\n    $0" "__len__" nil nil nil nil nil nil)
                       ("new" "def __new__(cls, name, bases, dict):\n    $0\n    return type.__new__(cls, name, bases, dict)\n" "__new__" nil nil nil nil nil nil)
                       ("setit" "def __setitem__(self, ${1:key}, ${2:val}):\n    $0" "__setitem__" nil nil nil nil nil nil)
                       ("ae" "self.assertEqual($1, $2)" "assertEqual" nil nil nil nil nil nil)
                       ("ae" "self.assertEqual($1, $2)" "assertEqual" nil nil nil nil nil nil)
                       ("af" "self.assertFalse($0)" "assertFalse" nil nil nil nil nil nil)
                       ("af" "self.assertFalse($0)" "assertFalse" nil nil nil nil nil nil)
                       ("ar" "self.assertRaises(${1:Exception}, ${2:fun})" "assertRaises" nil nil nil nil nil nil)
                       ("ar" "with self.assertRaises(${1:Exception}):\n     $0\n" "assertRaises" nil nil nil nil nil nil)
                       ("at" "self.assertTrue($0)" "assertTrue" nil nil nil nil nil nil)
                       ("__" "def __${1:$$(yas/choose-value '(\"str\" \"cmp\" \"iter\" \"eq\" \"repr\"))}__(self${2:, other}):\n    $0" "builtin" nil
                        ("definitions")
                        nil nil nil nil)
                       ("classd" "class ${1:ClassName}(${2:object}):\n    \"\"\"$3\n    \"\"\"\n\n    def __init__(self, $4):\n        \"\"\"$5\n        ${4:$\n        (let* ((indent\n                (concat \"\\n\" (make-string (current-column) 32)))\n               (args\n                (mapconcat\n                 '(lambda (x)\n                    (if (not (string= (nth 0 x) \"\"))\n                        (concat \"- \" (char-to-string 96) (nth 0 x)\n                                (char-to-string 96) \":\")))\n                 (mapcar\n                  '(lambda (x)\n                     (mapcar\n                      (lambda (x)\n                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))\n                  (mapcar '(lambda (x) (split-string x \"=\"))\n                          (split-string yas-text \",\")))\n                 indent)))\n          (if (string= args \"\")\n              (make-string 3 34)\n            (mapconcat\n             'identity\n             (list \"\" \"Arguments:\" args (make-string 3 34))\n             indent)))\n        }\n        ${4:$\n        (mapconcat\n         '(lambda (x)\n            (if (not (string= (nth 0 x) \"\"))\n                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))\n         (mapcar\n          '(lambda (x)\n             (mapcar\n              '(lambda (x)\n                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n              x))\n          (mapcar '(lambda (x) (split-string x \"=\"))\n                  (split-string yas-text \",\")))\n         (concat \"\\n\" (make-string (current-column) 32)))\n        }\n        $0\n" "class" nil nil nil nil nil nil)
                       ("@cm" "@classmethod\ndef ${1:method}(cls, $2):\n    $0\n    \n" "classmethod" nil nil nil nil nil nil)
                       ("class" "class ${1:name}:\n    def __init__(self, $2):\n        ${2:$\n        (mapconcat\n         '(lambda (x)\n            (if (not (string= (nth 0 x) \"\"))\n                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))\n         (mapcar\n          '(lambda (x)\n             (mapcar\n              '(lambda (x)\n                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n              x))\n          (mapcar '(lambda (x) (split-string x \"=\"))\n                  (split-string yas-text \",\")))\n         (concat \"\\n\" (make-string (current-column) 32)))\n        }\n        $0\n" "class" nil nil nil nil nil nil)
                       ("dec" "def ${1:decorator}(func):\n    $2\n    def _$1(*args, **kwargs):\n        $3\n        ret = func(*args, **kwargs)\n        $4\n        return ret\n    return _$1" "dec" nil
                        ("definitions")
                        nil nil nil nil)
                       ("defdoc" "def ${1:name}($2):\n    \"\"\"$3\n    ${2:$\n      (let* \n        ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string yas-text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0\n" "def" nil nil nil nil nil nil)
                       ("def" "def ${1:name}($2):\n    $0\n    return None\n" "def" nil nil nil nil nil nil)
                       ("def." "def ${1:name}(self${2:, $3}):\n    $0\n" "defm" nil nil nil nil nil nil)
                       ("ds" "'''$0.'''\n" "docstring" nil nil nil nil nil nil)
                       ("doct" ">>> ${1:function calls}\n${2:desired output}\n$0\n" "doctest" nil nil nil nil nil nil)
                       ("eq" "def __eq__(self, other):\n    return self.$1 == other.$1" "eq" nil
                        ("overloading")
                        nil nil nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil nil nil nil nil nil)
                       ("from" "from ${1:lib} import ${2:funs}" "from" nil
                        ("general")
                        nil nil nil nil)
                       ("f" "def ${1:function}($2):\n    $0\n    \n" "function" nil nil nil nil nil nil)
                       ("if" "if ${1:cond}:\n   $0" "if" nil
                        ("control structure")
                        nil nil nil nil)
                       ("ife" "if $1:\n   $2\nelse:\n   $0\n" "ife" nil
                        ("control structure")
                        ((yas/indent-line 'fixed))
                        nil nil nil)
                       ("ifmain" "if __name__ == '__main__':\n    $0" "if __name__ == '__main__': ..." nil nil nil nil nil nil)
                       ("ipdb" "import ipdb; ipdb.set_trace()" "ipdb" nil nil nil nil nil nil)
                       ("lam" "lambda ${1:x}: $0\n" "lambda" nil nil nil nil nil nil)
                       ("lc" "[${1:i} for $1 in ${2:list}]\n$0\n" "list" nil
                        ("definitions")
                        nil nil nil nil)
                       ("ln" "logger = logging.getLogger(__name__)" "logger_name" nil nil nil nil nil nil)
                       ("log" "logger = logging.getLogger(\"${1:name}\")\nlogger.setLevel(logging.${2:level})\n" "logging" nil nil nil nil nil nil)
                       ("m" "def ${1:name}(self${2:, $3}):\n    $0\n" "method" nil nil nil nil nil nil)
                       ("pdb" "import pdb; pdb.set_trace()" "pdb" nil nil nil nil nil nil)
                       ("pp" "import pprint; pprint.pprint($0)" "pprint" nil nil nil nil nil nil)
                       ("!!" "#!/usr/bin/env ${1:$$(yas/choose-value '(\"python\" \"python2\" \"python3\" \"python2.7\" \"python3.3\"))}\n# -*- coding: utf-8; -*-\n\n$0\n" "preamble" nil nil nil nil nil nil)
                       ("p" "print($0)" "print" nil nil nil nil nil nil)
                       ("prop" "def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n   def fset(self, value):\n       self._$1 = value\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0" "prop" nil nil nil nil nil nil)
                       ("propg" "def _get_${1:foo}(self):\n    return self._$1\n\n$1 = property(_get_$1)\n\n$0" "_get_foo ... foo=property(...)" nil nil nil nil nil nil)
                       ("propsg" "def _set_${1:foo}(self, value):\n    self._$1 = value\n\ndef _get_$1(self):\n    return self._$1\n\n$1 = property(_get_$1, _set_$1)\n\n$0" "_get_foo ... _set_foo ... foo=property(...)" nil nil nil nil nil nil)
                       ("ppdb" "import pudb; pudb.set_trace()\n" "pudb" nil nil nil nil nil nil)
                       ("reg" "${1:regexp} = re.compile(r\"${2:expr}\")\n$0" "reg" nil
                        ("general")
                        nil nil nil nil)
                       ("repr" "def __repr__(self):\n    $0" "repr" nil nil nil nil nil nil)
                       ("r" "return $0" "return" nil nil nil nil nil nil)
                       ("." "self.$0" "self" nil nil nil nil nil nil)
                       (".=" "self.$1 = $1\n" "selfassign" nil nil nil nil nil nil)
                       ("setdef" "${1:var}.setdefault(${2:key}, []).append(${3:value})" "setdef" nil nil nil nil nil nil)
                       ("@sm" "@staticmethod\ndef ${1:func}($2):\n    $0\n" "static" nil nil nil nil nil nil)
                       ("str" "def __str__(self):\n    $0" "str" nil
                        ("overloading")
                        nil nil nil nil)
                       ("super" "super(${1:Class}, self).${2:function}(${3:args})" "super" nil nil nil nil nil nil)
                       ("?:" "$1 if $2 else $0\n" "if" nil
                        ("control structure")
                        nil nil nil nil)
                       ("tcs" "class Test${1:toTest}(unittest.TestCase):\n      $0\n" "test_class" nil
                        ("definitions")
                        nil nil nil nil)
                       ("tf" "import unittest\n${1:from ${2:test_file} import *}\n\n$0\n\nif __name__ == '__main__':\n    unittest.main()" "test_file" nil
                        ("definitions")
                        nil nil nil nil)
                       ("try" "try:\n    $1\nexcept ${2:Exception} as e:\n    $0\n" "try" nil nil
                        ((yas/indent-line 'fixed))
                        nil nil nil)
                       ("try" "try:\n    $1\nexcept $2:\n    $3\nelse:\n    $0\n" "tryelse" nil nil
                        ((yas/indent-line 'fixed))
                        nil nil nil)
                       ("un" "def __unicode__(self):\n    $0" "unicode" nil nil nil nil nil nil)
                       ("utf" "# -*- encoding: utf-8 -*-\n$0" "utf8" nil
                        ("general")
                        nil nil nil nil)
                       ("while" "while ${condition}:\n    $0" "while ... : ..." nil nil nil nil nil nil)
                       ("with" "with ${1:expr}${2: as ${3:alias}}:\n     $0" "with" nil
                        ("control structure")
                        nil nil nil nil)))


;;; Do not edit! File generated at Sat Nov 22 15:54:20 2014
