(in-package :oliphaunt)


;;; Internationalization and Localization Support

(define-constant +language-names+
    '((:en "English" "English" "Language")
      (:es "Español" "Spanish" "Idioma")
      (:fr "Français" "French" "Langue")
      (:ga "Gaeilge" "Irish" "Teanga")
      (:ru "Русский" "Russian" "Язык")
      (:la "Lingua Latina" "Latin" "Lingua"))
  :test 'equalp)

(define-condition language-not-implemented-warning (warning)
  ((language :initarg :language :reader language-not-implemented)
   (fn :initarg :function :reader language-not-implemented-function))
  (:report (lambda (c s)
             (format s "There is not an implementation of ƒ ~A for language ~A ~@[~{~*(~A/~A)~}~]"
                     (slot-value c 'fn)
                     (slot-value c 'language)
                     (assoc (language-not-implemented c) +language-names+)))))

(defmacro defun-lang (function (&rest lambda-list) &body bodies)
  (let ((underlying (intern (concatenate 'string (string function) "%"))))
    #+test-i18n

    (let ((implemented (mapcar #'car bodies)))
      (unless (every (rcurry #'member implemented)
                     (mapcar #'car +language-names+))
        (warn "Defining ƒ ~A with partial language support: ~{~{~%  • ~5A: ~20A ~:[ ✗ ~; ✓ ~]~}~}"
              function
              (mapcar (lambda (language)
                        (list (car language)
                              (third language)
                              (member (car language) implemented)))
                      +language-names+))))

    `(progn
       (defgeneric ,underlying (language ,@lambda-list)
         ,@(mapcar (lambda (body)
                     (let ((match (car body))
                           (code (cdr body)))
                       (unless (assoc match +language-names+)
                         (warn "Defining a handler for unrecognized language-code ~A in ƒ ~A"
                               match function))
                       `(:method ((language (eql ,match)) ,@lambda-list)
                          ,@code)))
                   bodies)
         (:method ((language t) ,@lambda-list)
           (warn 'language-not-implemented-warning :language language :function ',function)
           (,underlying :en ,@lambda-list)))
       (defun ,function (,@lambda-list)
         (,underlying (or (get-lang) :en)
                      ,@lambda-list)))))

(defun char-string (char)
  (make-string 1 :initial-element char))

(defun irish-broad-vowel-p (char)
  (member char '(#\a #\o #\u #\á #\ó #\ú)))

(defun irish-broad-ending-p (word)
  (irish-broad-vowel-p (last-elt (remove-if-not #'irish-vowel-p word))))


;;; Genders of words which cannot be guessed by their declensions

(defvar irish-gender-dictionary (make-hash-table :test 'equal))
(dolist (word ($$$ adharc baintreach báisteach buíon caor cearc
                   ciall cloch cos craobh críoch cros dámh
                   dealbh eangach fadhb fearg ficheall fréamh
                   gaoth géag gealt girseach grian iall iníon
                   lámh leac long luch méar mian mias muc
                   nead pian sceach scian scornach slat
                   sluasaid srón téad tonn ubh banríon Cáisc
                   cuid díolaim Eoraip feag feoil muir scread
                   rogha teanga bearna veain beach))
  (setf (gethash word irish-gender-dictionary) :f))
(dolist (word ($$$ am anam áth béas bláth cath cíos cith
                   crios dath dream droim eas fíon flaith
                   greim loch lus luach modh rámh rang
                   rás roth rud sioc taom teas tréad
                   im sliabh ainm máistir seans club dlí
                   rince coláiste))
  (setf (gethash word irish-gender-dictionary) :m))


(defvar irish-declension-dictionary (make-hash-table :test 'equal))
(dolist (word ($$$ im sliabh
                   adharc baintreach báisteach buíon caor cearc ciall
                   cloch cos craobh críoch cros dámh dealbh eangach
                   fadhb fearg ficheall fréamh gaoth géag gealt
                   girseach grian iall iníon lámh leac long luch
                   méar mian mias muc nead pian sceach scian
                   scornach slat sluasaid srón téad tonn ubh))
  (setf (gethash word irish-declension-dictionary) 2))
(dolist (word ($$$ am anam áth béas bláth cath cíos cith crios
                   dath dream droim eas fíon flaith greim
                   loch lus luach modh rámh rang rás roth rud
                   sioc taom teas tréad
                   banríon Cáisc cuid díolaim Eoraip
                   feag feoil muir scread))
  (setf (gethash word irish-declension-dictionary) 3))
(dolist (word ($$$  rogha teanga bearna ainm máistir seans
                    club veain dlí rince))
  (setf (gethash word irish-declension-dictionary) 4))

(defvar english-gender-dictionary (make-hash-table :test 'equal))
(dolist (word '(car automobile ship plane
                airplane boat vessel
                cat kitty hen chick peahen
                girl woman lady miss mistress mrs ms
                chauffeuse masseuse stewardess
                madam))
  (setf (gethash (string word) english-gender-dictionary) :f))
(dolist (word '(man boy guy bloke fellow dude
                dog cock rooster peacock
                mister master mr))
  (setf (gethash (string word) english-gender-dictionary) :m))


(defun latin-normalize (string)
  (regex-replace-pairs '(("i" "j")
                         ("v" "u")
                         ("w" "uu")
                         ("æ" "ae")
                         ("œ" "oe"))
                       (string-downcase string)))

(defun latin-presentation-form (string)
  (funcall (letter-case string)
           (regex-replace-pairs '(("ae" "æ")
                                  ("oe" "œ")
                                  ("u" "v"))
                                (string-downcase string))))


;; Determine the gender and declension of a word

(defun-lang gender-of (noun)
  (:en (if-let ((gender (gethash (string-upcase noun) english-gender-dictionary)))
         gender
         :n))
  (:es (string-ends-with-case noun
         ("o" :m)
         ("a" :f)
         (otherwise :m)))
  (:fr (if (member (last-elt noun) '(#\e #\E))
           :f
           :m))
  (:la (or (gethash (latin-normalize noun) latin-gender-dictionary)
           (case (declension-of noun)
             (1 :f)
             (2 (string-ends-with-case noun
                  ("us" :m)
                  ("um" :n)))
             (3 :m) ; totally random guess
             (4 :f)
             (5 :f))))
  (:ga (or (gethash (string-downcase noun) irish-gender-dictionary)
           (string-ends-with-case noun
             (("e" "í") :f)
             (($$$ a o e u i
                   á ó é ú í
                   ín) :m)
             (($$$ áil úil ail úint cht irt) :f)
             (($$$ éir eoir óir úir) :m)
             (("óg" "eog" "lann") :f)
             (otherwise
              (if (irish-broad-ending-p noun)
                  :m
                  :f))))))

(defun-lang declension-of (noun)
  (:en nil)
  (:la (if-let (genitive (gethash (latin-normalize noun) latin-genitives))
         (string-ends-with-case genitive
           ("ae" 1)
           ("ēī" 5)
           ("ī" 2)
           ("is" 3)
           ("ūs" 4)))
       (string-ends-with-case noun      ; bad fallback
         ("a" 1)
         ("us" 2)                       ; could be 4, but less likely…
         ("um" 2)
         ("ēs" 5)
         (otherwise 3)))
  (:ga (if-let ((overrule (gethash noun irish-declension-dictionary)))
         overrule
         (cond
           ((or (eql (last-elt noun) #\e)
                (eql (last-elt noun) #\í)
                (irish-vowel-p (last-elt noun))
                (string-ends "ín" noun)) 4)
           ((or (member noun '("áil" "úil" "ail" "úint" "cht" "irt"
                               "éir" "eoir" "óir" "úir")
                        :test #'string-ending)) 3)
           ((or (string-ends "eog" noun)
                (string-ends "óg" noun)
                (string-ends "lann" noun)
                (not (irish-broad-ending-p noun))) 2)
           ((irish-broad-ending-p noun) 1)
           (t (warn "Can't guess declension () of “~a”" noun))))))

(defvar latin-genitives (make-hash-table :test #'equal))
(defvar latin-gender-dictionary (make-hash-table :test #'equal))

(let ((array (mapcar (lambda (word)
                       (let ((parts (split-sequence #\space (substitute #\space #\, word) :remove-empty-subseqs t :count 3)))
                         (if (char= (elt (elt parts 1) 0) #\-)
                             (list (elt parts 0) (keyword* (elt parts 1)) (keyword* (elt parts 2)))
                             (list (elt parts 0) (elt parts 1) (keyword* (elt parts 2))))))
                     '("accola -ae m"
                       "advena -ae m"
                       "agricola, -ae, m"
                       "agripeta, -ae m"
                       "alienigena -ae m"
                       "alipta -ae m"
                       "aliptes aliptae m"
                       "amniclola -ae m"
                       "anagnostes anagnostae m"
                       "analecta, -ae m"
                       "anguigena, -ae m"
                       "anthias, -ae m"
                       "archipirata, -ae m"
                       "artopta, -ae m"
                       "athleta, -ae m"
                       "auriga, -ae m"
                       "Abnoba, -ae m"
                       "Acestes, Acestae m"
                       "Achates, -ae m"
                       "Acmonides, -ae m"
                       "Actorides, -ae m"
                       "Aeeta, -ae m"
                       "Aeneas, -ae m"
                       "Aenides, -ae m"
                       "Agamemnonides, -ae m"
                       "Agrippa, -ae m"
                       "Ahala, -ae m"
                       "Amisia, -ae m"
                       "Amphiaraides, -ae m"
                       "Ampycides, -ae m"
                       "Amyntas, -ae m"
                       "Amyntiades, -ae m"
                       "Anas, Anae m"
                       "Anaxagoras, -ae m"
                       "Anchises, -ae m"
                       "Anchisiades, -ae m"
                       "Antiphates, -ae m"
                       "Antisthenes, -ae m"
                       "Aonides, -ae m"
                       "Apolloniates, -ae m"
                       "Appenninicola, -ae c"
                       "Appenninigena, -ae c"
                       "Arabarches, -ae m"
                       "Archias, -ae m"
                       "Arestorides, -ae m"
                       "Asopiades, -ae m"
                       "Astacides, -ae m"
                       "Athamantiades, -ae m"
                       "Atlantiades, -ae m"
                       "Atrida Atridae m"
                       "Atrides, Atridae m"
                       "Atta, -ae m"
                       "Aurigena, -ae m"
                       "Axona, -ae m"
                       "brabeuta, -ae m"
                       "bucaeda, -ae m"
                       "Bacchiadae, -ārum m"
                       "Bagoas, -ae m"
                       "Bagrada, -ae m"
                       "Baptae, -ārum m"
                       "Barcas, -ae m"
                       "Bastarnae -ārum m"
                       "Basternae, -ārum m"
                       "Battiades, -ae m"
                       "Belgae, -ārum m"
                       "Bellerophontes, -ae m"
                       "Belides, -ae m"
                       "Bootes, -ae m"
                       "Boreas, -ae m"
                       "cacula, -ae m"
                       "caecias, -ae m"
                       "cataphractes, -ae m"
                       "cerastes, -ae m"
                       "choraules, -ae m"
                       "citharista, -ae m"
                       "clepta, -ae m"
                       "cometes, -ae m"
                       "conchita, -ae m"
                       "conlega, -ae m"
                       "convenae, -ārum c"
                       "conviva, -ae m"
                       "coprea, -ae m"
                       "Caligula, -ae m"
                       "Caracalla, -ae m"
                       "Catilina, -ae m"
                       "Cecropides, -ae m"
                       "Celtae, -ārum m"
                       "Charondas, -ae m"
                       "Chrysas, -ae m"
                       "Chryses, -ae m"
                       "Cinga, -ae m"
                       "Cinna, -ae m"
                       "Cinyras, -ae m"
                       "Clinias, -ae m"
                       "Cliniades, -ae m"
                       "Columella, -ae m"
                       "Cotta, -ae m"
                       "Crotoniates, -ae m"
                       "Crotopiades, -ae m"
                       "danista, -ae m"
                       "dioecetes, -ae m"
                       "draconigena, -ae m"
                       "drapeta, -ae m"
                       "Dalmatae, -ārum m"
                       "Dolabella, -ae m"
                       "etesiae, -ārum m"
                       "Eleates, -ae m"
                       "Eumolpidae, -ārum m"
                       "faeniseca, -ae m"
                       "fratricida, -ae m"
                       "geometres, -ae m"
                       "grammatista, -ae m"
                       "gumia, -ae m"
                       "Galatae, -ārum m"
                       "Galba, -ae m"
                       "Gangaridae, -ārum m "
                       "Geta, -ae, m"
                       "Gorgias, -ae m"
                       "Graiugena, -ae m"
                       "Gyas, -ae m"
                       "Gyges, -ae m"
                       "halophanta, -ae m"
                       "heuretes, -ae m"
                       "hybrida hybridae m"
                       "hibrida -ae m"
                       "hippotoxota, -ae m"
                       "homicida, -ae c"
                       "Heraclides, -ae m"
                       "Herma -ae m"
                       "Hermes Hermae m"
                       "Hilotae, -ārum m"
                       "Ilotae Itolārum m"
                       "Hippias, -ae m"
                       "Hippomenes, -ae m"
                       "Hippotades, -ae m"
                       "ignigena, -ae m"
                       "incola, -ae m"
                       "Ianigena, -ae m"
                       "Iarbas (Iarba), -ae"
                       "Iliades, -ae m"
                       "Iuba, -ae m"
                       "Iugurtha, -ae m"
                       "Iura, -ae m"
                       "lanista, -ae m"
                       "latebricola, -ae m"
                       "lixa, -ae m"
                       "Ladas, -ae m"
                       "Lamia, -ae m"
                       "Lapithae, -ārum m"
                       "Leonidas, -ae m"
                       "nauta, -ae m"
                       "parricida, -ae m"
                       "pirata, -ae m"
                       "poeta, -ae m"
                       "Proca, -ae m"
                       "tata, -ae m"
                       "umbraticola, -ae m"
                       "xiphias, -ae m"
                       ))))
  (flet ((apply-genitive (gen* nom)
           (etypecase gen*
             (string gen*)
             (keyword (ecase gen*
                        (:-ae (if (char= (last-elt nom) #\a)
                                  (strcat nom "e")
                                  (strcat nom "ae")))
                        ((:-arum :-ārum) (strcat (if (string-ends "ae" nom)
                                                     (subseq nom (- (length nom) 2))
                                                     nom)
                                                 "ārum"))
                        ((:-ī :-i) (strcat (if (or (string-ends "um" nom)
                                                   (string-ends "us" nom))
                                               (subseq nom 0 (- (length nom) 2))
                                               nom)
                                           "ī"))
                        ((:-orum :-ōrum) (strcat (if (string-ends "ae" nom)
                                                     (subseq nom (- (length nom) 2))
                                                     nom)
                                                 "ōrum"))
                        (:-is (strcat nom "is"))
                        (:-ium (strcat nom "ium"))
                        ((:-us :-ūs) (strcat (if (string-ends "us" nom)
                                                 (subseq nom 0 (- (length nom) 2))
                                                 nom)
                                             "ūs"))
                        ((:-ei :-ēī) (if (string-ends "ēs" nom)
                                         (strcat (subseq nom 0 (- (length nom) 1)) "ī")
                                         (strcat nom "ēī"))))))))
    (dolist (word array)
      (destructuring-bind (nom gen* &optional gender) word
        (let ((gen (apply-genitive gen* nom)))
          (setf (gethash (latin-normalize nom) latin-genitives) (latin-normalize gen))
          (when gender
            (setf (gethash (latin-normalize nom) latin-gender-dictionary) gender)))))))

(let ((words ($$$ aidiacht aiste anáil bacach bád bádóir
                  báicéir baincéir bainis béal buidéal caint
                  cat céad ceadúnas ceann ceart cinnúint
                  cléreach cliabh cogadh coileach coláiste
                  comhairle deis dochtúir))
      (genders (list :f :f :f :m :m
                     :m :m :m :f :m :m
                     :f :m :m :m :m :m
                     :f :m :m :m :m
                     :m :f :f :m))
      (declensions (list 3 4 3 1 1
                         3 3 3 2 1
                         1 2 1 1 1 1
                         1 3 1 1 1 1
                         4 4 2 3)))
  (loop for word in words
     for gender in genders
     for declension in declensions
     for c-g = (gender-of% :ga word)
     for c-decl = (declension-of% :ga word)
     do (assert (and (eql gender c-g) (eql declension c-decl))
                nil
                "~a is ~a, ~:r declension (computed ~:[✗~;✓~]~a ~:[✗~;✓~], ~:r declension)"
                word gender declension
                (eql gender c-g) (or c-g "could not decide")
                (eql declension c-decl) (or c-decl "could not compute"))))


;;; Presentation and internalized forms.

(defun internalize-irish (word)
  "Remove presentation forms for Irish"
  (substitute-map '(#\ı #\i
                    #\ɑ #\a
                    #\⁊ #\&) word))

(defun present-irish (word)
  "Create a “read-only” string that looks nicer, including the use of
dotless-i (ı) and the letter “latin alpha,“ (ɑ), the Tironian ampersand,
and fixing up some irregular hyphenation rules.

This is the preferred (although frequently enough, not the observed)
presentation form for Irish."
  (let ((word1 (substitute-map '(#\i #\ı
                                 #\a #\ɑ
                                 #\A #\Ɑ
                                 #\⁊ #\&) word)))
    (when (or (search "t-" word1)
              (search "n-" word1))
      (setf word1 (cl-ppcre:regex-replace "\\b([tn])-([AOEUIÁÓÉÚÍ])" word1
                                          "\\1\\2")))
    (when (or (search "de " word1)
              (search "do " word1)
              (search "me " word1)
              (search "ba " word1))
      (setf word1 (cl-ppcre:register-groups-bind
                      (prelude preposition fh initial after)
                      ("^(.*)\\b(ba|de|mo|do)\\s+(fh)?([aoeuiAOEUIáóéúíÁÓÉÚÍ])(.*)$"
                       word1)
                    (strcat prelude (char-string (elt preposition 0))
                            "'" ; apostrophe
                            fh initial after))))))

(defun irish-eclipsis (word)
  "In certian grammatical constructions, the first consonant of an Irish
word may be “eclipsed” to change its sound. "
  (strcat (case (elt word 0)
            (#\b "m")
            (#\c "g")
            (#\d "n")
            (#\f "bh")
            (#\g "n")
            (#\p "b")
            (#\t "d")
            ((#\a #\o #\e #\u #\i
                  #\á #\ó #\é #\ú #\í) "n-")
            (otherwise "")) word))

(defun irish-downcase-eclipsed (word)
  "In Irish, eclipsis-added characters shouldn't be capitalized with the
rest of the word; e.g. as an “bPoblacht.“

It's technically allowed, but discouraged, in ALL CAPS writing."
  (cond
    ((member (subseq word 0 2)
             '("MB" "GC" "ND" "NG" "BP" "DT")
             :test 'string-equal)
     (strcat (string-downcase (subseq word 0 1))
             (funcall (letter-case word) (subseq word 1))))
    ((string-equal (subseq word 0 3) "BHF")
     (strcat "bh"
             (funcall (letter-case word) (subseq word 2))))
    (t word)))

(assert (equal (irish-downcase-eclipsed "Bpoblacht") "bPoblacht"))
(assert (equal (irish-downcase-eclipsed "BHFEAR") "bhFEAR"))

(defmacro with-irish-endings ((word) &body body)
  `(block irish-endings
     (let* ((last-vowel (or (position-if #'irish-vowel-p ,word :from-end t)
                            (progn
                              (warn "No vowels in ,word? ~A" ,word)
                              (return-from irish-endings ,word))))
            (last-vowels-start
             (if (and (plusp last-vowel)
                      (irish-vowel-p (elt ,word (1- last-vowel))))
                 (if (and (< 1 last-vowel)
                          (irish-vowel-p (elt ,word (- last-vowel 2))))
                     (- last-vowel 2)
                     (1- last-vowel))
                 last-vowel))
            (vowels (subseq ,word last-vowels-start (1+ last-vowel)))
            (ending (subseq ,word last-vowels-start)))
       (flet ((replace-vowels (replacement)
                (strcat (subseq ,word 0 last-vowels-start) replacement
                        (subseq ,word (1+ last-vowel))))
              (replace-ending (replacement)
                (strcat (subseq ,word 0 last-vowels-start) replacement))
              (add-after-vowels (addition)
                (strcat (subseq ,word 0 (1+ last-vowel)) addition
                        (subseq ,word (1+ last-vowel)))))
         ,@body))))

(defun caolú (word)
  "Caolú is the Irish version of palatalisation."
  (with-irish-endings (word)
    (cond
      ((= (length word) last-vowel) word)
      ((or (string-equal "each" ending)
           (string-equal "íoch" ending)) (replace-ending "igh"))
      ((string-equal "ach" ending) (replace-ending "aigh"))
      (t
       (string-case vowels
         (("éa" "ia") (replace-vowels "éi"))
         ("ea" (replace-vowels "i")) ;; or ei; when?
         (("io" "iu") (replace-vowels "i"))
         ("ío" (replace-vowels "í"))
         (otherwise
          (add-after-vowels "i")))))))

(assert (equalp (mapcar #'caolú
                        '("leanbh" "fear" "cliabh" "coileach" "leac"
                          "ceart" "céad" "líon" "bacach" "pian"
                          "neart" "léann" "míol" "gaiscíoch"))
                '("linbh" "fir" "cléibh" "coiligh" "lic"
                  "cirt" "céid" "lín" "bacaigh" "péin"
                  "nirt" "léinn" "míl" "gaiscigh")))

;;; NOTE: féar→fir is asserted, but I think they meant fear→fir?
;;; ditto for gaiscíoch→gaiscígh (gaiscigh)

(defun coimriú (word)
  "Irish syncopation (shortening a syllable)"
  (let* ((last-vowel-pos (position-if #'irish-vowel-p word :from-end t))
         (ending-start (loop with index = last-vowel-pos
                          if (irish-vowel-p (elt word index))
                          do (setf index (1- index))
                          else return (1+ index))))
    (if (or (= 1 (syllable-count% :ga word))
            (= (1- (length word)) last-vowel-pos)
            (find (elt word last-vowel-pos) "áóéúí")
            ;; (long-vowel-p% :ga (subseq word ending-start)) ?
            (not (find (elt word (1- ending-start)) "lmnr")))
        word
        (let* ((tail (subseq word (1+ last-vowel-pos)))
               (tail (if (member tail '("rr" "ll" "mm") :test 'string-equal)
                         (subseq tail 0 1)
                         tail))
               (prefix (subseq word (1- ending-start))))

          (cl-ppcre:regex-replace
           "(dn|nd)"
           (cl-ppcre:regex-replace
            "(ln|nl|dl)" ;; dl retained in writing?
            (strcat prefix tail)
            "ll")
           "nn")))))

(defun leathnú (word)
  "Make a word broad (in Irish)"
  (with-irish-endings (word)
    ;; (format *trace-output* "~& Leathnú word ~A vowels ~A"
    ;;         word vowels)
    (let ((base (string-case vowels
                  (("ei" "i") (replace-vowels "ea"))
                  ("éi" (replace-vowels "éa"))
                  ("í" (replace-vowels "ío"))
                  ("ui" (replace-vowels "o"))
                  ("aí" (replace-vowels "aío"))
                  (otherwise
                   (if (and (< 1 (length vowels))
                            (char= #\i (last-elt vowels)))
                       (replace-vowels (subseq vowels 0 (1- (length vowels))))
                       word)))))
      (strcat base
              (case (last-elt base)
                ((#\r #\l #\m #\n) "a")
                (otherwise ""))))))

(let ((slender-words '("múinteoir" "bliain" "feoil" "dochtúir" "fuil"
                       "baincéir" "greim" "móin" "altóir" "muir"))
      (broad-words '("múinteora" "bliana" "feola" "dochtúra" "fola"
                     "baincéara" "greama" "móna" "altóra" "mara")))
  (unless (equalp (mapcar #'leathnú slender-words) broad-words)
    (#+irish-cerror
     cerror
     #+irish-cerror
     "Continue, and look foolish when speaking Irish"
     #-irish-cerror
     warn

     "The LEATHNÚ function (used in Irish grammar) is being tested
with a set of known-good word-forms, but something has gone awry
and it has failed to properly “broaden” the ending of one or
more of the words in the test set.

Slender forms: ~{~A~^, ~}
Computed broad forms: ~{~A~^, ~}
Correct broad forms: ~{~A~^, ~}"
     slender-words (mapcar #'leathnú slender-words) broad-words)))


(defun leathnaítear (word)
  "LEATHNAÍTEAR (lenition) is used to change the leading consonant in
certain situations in Irish grammar.

This does NOT enforce the dntls+dts nor m+bp exceptions.

Note that LEATHNÚ applies this to the final consonant, instead."
  (flet ((lenite ()
           (strcat (subseq word 0 1)
                   "h"
                   (subseq word 1))))
    (cond
      ((member (elt word 0) '(#\b #\c #\d #\f #\g #\m #\p #\t))
       (lenite))
      ((and (char= #\s (elt word 0))
            (not (member (elt word 1) '(#\c #\p #\t #\m #\f))))
       (lenite))
      (t word))))


(defun-lang syllable-count (string)
  (:en (loop
          with counter = 0
          with last-vowel-p = nil

          for char across (string-downcase string)
          for vowelp = (member char '(#\a #\o #\e #\u #\i))

          when (or (and vowelp
                        (not last-vowel-p))
                   (member char '(#\é #\ö #\ï)))
          do (incf counter)

          do (setf last-vowel-p vowelp)

          finally (return (max 1
                               (if (and (char= char #\e)
                                        (not last-vowel-p))
                                   (1- counter)
                                   counter)))))
  (:es (loop
          with counter = 0
          with last-i-p = nil

          for char across (string-downcase string)
          for vowelp = (member char '(#\a #\o #\e #\u #\i))

          when (or (and vowelp
                        (not last-i-p))
                   (member char '(#\á #\ó #\é #\ú #\í)))
          do (incf counter)

          do (setf last-i-p (eql char #\i))

          finally (return (max 1 counter))))

  (:la (loop
          with counter = 0

          for char across (latin-normalize string)
          when (member char '(#\a #\o #\e #\u #\i))
          do (incf counter)

          when (member char '(#\ā #\ē #\ī #\ō #\ū))
          do (incf counter 2)

          finally (return (max 1 counter))))

  (:ga (loop
          with counter = 0
          with last-vowel-p = nil

          for char across (string-downcase string)
          for vowelp = (irish-vowel-p char)

          when (and vowelp
                    (not last-vowel-p))
          do (incf counter)

          do (setf last-vowel-p vowelp)

          finally (return (max 1 counter)))))

(defun-lang diphthongp (letters)
  (:en (member (string-downcase letters)
               ($$$ ow ou ie igh oi oo ea ee ai) :test #'string-beginning))
  (:la (member (string-downcase letters)
               ($$$ ae au ai ou ei) :test #'string-beginning))
  (:ga (member (string-downcase letters)
               ($$$ ae eo ao abh amh agh adh)
               :test #'string-beginning)))

(defun vowelp (letter)
  (find letter "aoeuiáóéúíýàòèùìỳäöëüïÿāōēūīãõẽũĩỹąęųįøæœåŭαοευιωаоеуийюяэыё"))

(defun-lang long-vowel-p (syllable)
  (:la (let* ((first-vowel-pos (position-if #'vowelp syllable))
              (first-vowel (elt syllable first-vowel-pos)))
         (and first-vowel-pos
              (or (find first-vowel "āōēūī" :test #'char=)
                  (diphthongp% :la (subseq syllable first-vowel-pos))))))
  (:ru (some (rcurry #'find "юяиеёь" :test #'char=) syllable))
  (:en (if (and (= 2 (length syllable))
                (not (vowelp (elt syllable 0)))
                (alpha-char-p (elt syllable 0))
                (vowelp (elt syllable 1)))
           ;; be, by, so, to, et al.
           t
           (let* ((first-vowel-pos (position-if #'vowelp syllable))
                  (first-vowel (elt syllable first-vowel-pos)))
             (and first-vowel-pos
                  (or (find first-vowel "āōēūī" :test #'char=)
                      (and (find first-vowel "aoeui")
                           (or (and (< (1+ first-vowel-pos) (length syllable))
                                    (find (elt syllable (1+ first-vowel-pos))
                                          "aoeuiy"))
                               (and (< (+ 2 first-vowel-pos) (length syllable))
                                    (find (elt syllable (+ 2 first-vowel-pos))
                                          "aoeuiy")
                                    (not (eql #\w (elt syllable (1+ first-vowel-pos))))
                                    (not (eql #\x (elt syllable
                                                       (1+ first-vowel-pos))))))))))))
  (:ga (etypecase syllable
         (character
          (find syllable "áóéúí"))
         (string
          (let* ((first-vowel-pos (position-if #'irish-vowel-p syllable))
                 (first-vowel (elt syllable first-vowel-pos)))
            (or (find first-vowel "áóéúí")
                (diphthongp% :ga (subseq syllable first-vowel-pos))))))))



(defun irish-plural-form (string)
  (let* ((gender (gender-of% :ga string))
         (declension (declension-of% :ga string))
         (syllables (syllable-count% :ga string))
         (multi-syllabic (< 1 syllables))
         (len (length string)))
    ;; (format t "~& ~a = ~:r decl. ~:a, ~r syllable~:p"
    ;;         string declension gender syllables)
    (string-case string
      ;; some very irregular ones
      ("seoid"  "seoda")
      ("bean"  "mná")
      ("grasta" "grásta")
      ;; oliphaunt: probably irregulars?
      ("súil" "súila")
      ("deoir" "deora")
      ("cuibreach" "cubraigha")
      ;; oliphaunt: there are a few more irregulars to add, too.
      (otherwise
       (flet ((lessen (less)
                (subseq string 0 (- len less))))
         (cond
           ((and (= 4 declension) ; 4* -iú
                 (string-ends "iú" string))
            ;; observed, oliphaunt
            (strcat (lessen 2) "ithe"))
           ((and (= 4 declension) ; 4* -ú
                 (eql (last-elt string) #\ú))
            ;; observed, oliphaunt
            (strcat (lessen 1) "uíthe"))
           ((and (= 4 declension) ; 4♀ [rlnm]í
                 (eq :f gender)
                 (member (elt string (- len 2))
                         '(#\r #\l #\n #\m))
                 (eql (last-elt string) #\í))
            (strcat (lessen 1) "ithe"))
           ((and (= 4 declension) ; 4♀ [íe]
                 (eq :f gender)
                 (or (eql (last-elt string) #\í)
                     (eql (last-elt string) #\e))
                 (not (eql (elt string (- len 2)) #\t))
                 (not (eql (elt string (- len 2)) #\l)))
            (strcat (lessen 1) "t" (last-elt string)))

           ((and (= 4 declension) ; 4 - a
                 (eql (last-elt string) #\a))
            (strcat string "í"))
           ((and (not multi-syllabic) ; 1♂, 2♀ long+r  (1-syl.)
                 (or (and (eq :m gender)
                          (= 1 declension))
                     (and (eq :f gender)
                          (= 2 declension)))
                 (char= #\r (last-elt string))
                 (or (diphthongp% :ga (subseq string
                                              (- len 2)
                                              len))
                     (long-vowel-p% :ga (elt string (- len 1)))))

            (strcat string "tha")       ; or -the
            )
           ((and (not multi-syllabic) ; 1♂,2♀ long+[ln]  (1-syl.)
                 (or (and (eq :m gender)
                          (= 1 declension))
                     (and (eq :f gender)
                          (= 2 declension))
                     (= 3 declension))
                 (or (char= #\l (last-elt string))
                     (char= #\n (last-elt string)))
                 (or (diphthongp% :ga (subseq string
                                              (1+ (or
                                                   (position-if (complement #'irish-vowel-p)
                                                                (subseq string 0 (1- len))
                                                                :from-end t)
                                                   -1))
                                              len))
                     (long-vowel-p% :ga (elt string (- len 1)))))
            (strcat string "ta")        ; or -te?
            )
           ((or (and (= 2 declension) ; 2♀ -oeg &c; 1♀ -ach; 3/4*
                     (eq :f gender)
                     (or (string-ends "eog" string)
                         (string-ends "óg" string)
                         (string-ends "lann" string)
                         (and multi-syllabic
                              (string-ends "each" string))
                         (equal string "binn")
                         (equal string "deoir")))
                (and (= 1 declension)
                     (eq :f gender)
                     (string-ends "ach" string))
                (= 3 declension)
                (= 4 declension))
            (leathnú string))
           ((and multi-syllabic ; 1♂,2♀ -ach, 3* -éir &c, 4* -ín,-a,-e (mult.)
                 (or ;; (and (eq :m gender)
                  ;;      (= 1 declension)
                  ;;      (or (string-ends "adh" string)
                  ;;          (string-ends "ach" string)))
                  (and (eq :f gender)
                       (= 2 declension)
                       (or (not (irish-broad-ending-p string))
                           (string-ends "ach" string)))
                  (and (= 3 declension)
                       (member string
                               '("éir" "eoir" "óir" "úir"
                                 "cht" "áint" "úint" "irt")
                               :test #'string-ending))
                  (and (= 4 declension)
                       (or (string-ends "ín" string)
                           (string-ends "a" string)
                           (string-ends "e" string)))))
            (strcat string "í")
            ;; rules read:
            ;; • add -(a)í
            ;; • -(e)adh, -(e)ach → (a)í
            ;; • -e → í

            )
           ((and (= 2 declension) ; 2♀
                 (eq :f gender))
            (strcat string
                    (if (irish-broad-ending-p string)
                        "a"
                        "e")))

           ((and (not multi-syllabic) ; 1♂,2♀,3♂,4* — (1-syl)
                 (or (and (eq :m gender)
                          (= 1 declension)
                          (not (irish-broad-ending-p string)))
                     (and (eq :f gender)
                          (= 2 declension)
                          (not (irish-broad-ending-p string)))
                     (and (eq :m gender)
                          (= 3 declension))
                     (= 4 declension)))
            (strcat string "anna")      ; -(e)anna??
            )

           ((or (and (= 2 syllables) ; 1♂, -[lnr]; 2♀*, 3 -i[lnr], 4* — 2 syl.
                     (eq :m gender)
                     (= 1 declension)
                     (let ((last (last-elt string)))
                       (or (char= #\l last)
                           (char= #\n last)
                           (char= #\r last))))
                (and (= 2 declension)
                     (eq :f gender))
                (and (= 3 declension)
                     (or (string-ends "il" string)
                         (string-ends "in" string)
                         (string-ends "ir" string)))
                (= 4 declension))

            (strcat string "acha") ; or -eacha
            )

           ((and (= 1 declension) ; 1♂*
                 (eq :m gender))
            (caolú string)
            #+ (or)
            ((and (= 1 declension)
                  (eq :m gender))
;;; XXX probably special-case based on ending?
             (strcat (coimriú string) "e")))

           ((= 3 declension) ; 3*
            (strcat (coimriú string) "a"))

           (t (warn "Unable to figure out the plural form of “~A” (~:R decl. ~A)"
                    string declension gender)
              string)))))))


;; English exceptional plurals dictionaries.


(defvar english-defective-plurals (make-hash-table :test 'equal)
  "Words with no actual plural forms")
(dolist (word ($$$ bison buffalo deer duck fish moose
                   pike plankton salmon sheep squid swine
                   trout algae marlin
                   furniture information
                   cannon blues iris cactus
                   meatus status specie
                   benshi otaku samurai
                   kiwi kowhai Māori Maori
                   marae tui waka wikiwiki
                   Swiss Québécois omnibus
                   Cherokee Cree Comanche Delaware Hopi
                   Iroquois Kiowa Navajo Ojibwa Sioux Zuni))
  (setf (gethash word english-defective-plurals) word))

(defvar english-irregular-plurals (make-hash-table :test 'equal)
  "Words whose plurals cannot be guessed using the heuristics")
(defvar english-irregular-plurals-reverse (make-hash-table :test 'equal)
  "Words whose plurals cannot be guessed using the heuristics")
(loop for (s pl) in '(("child" "children") ("ox" "oxen")
                      ("cow" "kine") ("foot" "feet")
                      ("louse" "lice") ("mouse" "mice")
                      ("tooth" "teeth") ("die" "dice") ("person" "people")
                      ("genus" "genera") ("campus" "campuses")
                      ("viscus" "viscera") ("virus" "viruses")
                      ("opus" "opera") ("corpus" "corpera")
                      ("cherub" "cherubim")
                      ("person" "people") ; contentious, but usually right
                      ("seraph" "seraphim") ("kibbutz" "kibbutzim")
                      ("inuk" "inuit") ("inukshuk" "inukshuit")
                      ("Iqalummiuq" "Iqalummiut")
                      ("Nunavimmiuq" "Nunavimmiut")
                      ("Nunavummiuq" "Nunavummiut")
                      ("aide-de-camp" "aides-de-camp"))
   do (setf (gethash s english-irregular-plurals) pl)
   do (setf (gethash pl english-irregular-plurals-reverse) s))

(defun make-english-plural (string)
  "Attempt to pluralize STRING using some heuristics that should work
well enough for many (most) English words. At least, an improvement upon
~:P …"
  (when (search "person" string :test #'char-equal)
    (setf string (regex-replace-pairs '(("PERSON" . "PEOPLE")
                                        ("person" . "people")) string)))
  (funcall (letter-case string)
           (let ((s (string-downcase string)))
             (flet ((lessen (n)
                      (subseq s 0 (- (length s) n))))
               (or (gethash s english-defective-plurals)
                   (gethash s english-irregular-plurals)
                   (string-ends-with-case s
                     ;; Naturally, all of these are completely hueristic
                     ;; and often incomplete, but they appear to cover
                     ;; most irregular words without affecting too very
                     ;; many words that they shouldn't.
                     ("penny" (strcat (lessen 4) "ence"))
                     ("eau" (strcat s "x"))
                     (("ies" "ese" "fish") s)
                     ("ife" (strcat (lessen 2) "ves"))
                     (("eef" "eaf" "oaf") (strcat (lessen 1) "ves"))
                     ("on" (strcat (lessen 2) "a"))
                     ("ma" (strcat s "ta"))
                     (("ix" "ex") (strcat (lessen 1) "ces"))
                     ("nx" (strcat (lessen 1) "ges"))
                     (("tum" "dum" "rum") (strcat (lessen 2) "a"))
                     (("nus" "rpus" "tus" "cus" "bus"
                             "lus" "eus" "gus" "mus") (strcat (lessen 2) "i"))
                     (("mna" "ula" "dia") (strcat (lessen 1) "ae"))
                     ("pus" (strcat (lessen 2) "odes"))
                     ("man" (strcat (lessen 2) "en"))
                     (("s" "x") (strcat s "es"))
                     ("ey" (strcat s "s"))
                     ("y" (let ((penult (elt s (- (length s) 2)))
                                (antepenult (elt s (- (length s) 3))))
                            (if (and (or (eql #\r penult) (char= #\l penult))
                                     (vowelp antepenult))
                                (strcat (lessen 1) (char-string penult) "ies")
                                (strcat (lessen 1) "ies"))))

                     (otherwise
                      (strcat s "s"))))))))

(defun make-english-singular (string)
  (when (search "people" string :test #'char-equal)
    (setf string (regex-replace-pairs '(("PEOPLE" . "PERSON")
                                        ("people" . "person")) string)))
  (funcall (letter-case string)
           (let ((s (string-downcase string)))
             (flet ((lessen (n)
                      (subseq s 0 (- (length s) n))))
               (or (gethash s english-defective-plurals)
                   (gethash s english-irregular-plurals-reverse)
                   (string-ends-with-case s
                     ("pence" (strcat (lessen 4) "enny"))
                     ("eaux" (lessen 1))
                     (("ese" "fish") s)
                     (("eeves" "eaves" "oaves") (strcat (lessen 3) "f"))
                     ("ives" (strcat (lessen 3) "fe"))
                     ("mata" (lessen 2))
                     ("oices" (lessen 1))
                     (("eces" "ices") (strcat (lessen 3) "x"))
                     (("ynges" "anges") (strcat (lessen 3) "x"))
                     ("ae" (lessen 1))
                     ("a" (strcat (lessen 1) "um")) ; could have easily been "on" though.
                     ("i" (strcat (lessen 1) "us"))
                     ("podes" (strcat (lessen 4) "us"))
                     ("men" (strcat (lessen 2) "an"))
                     ("im" (lessen 2))
                     (("ses" "xes") (lessen 2))
                     ("ies" (strcat (lessen 3) "y"))
                     ("s" (lessen 1))
                     (otherwise s)))))))

(loop for (sing pl) on ($$$
                        person-in-place people-in-places
                        country countries
                        monkey monkeys
                        penny pence
                        corpus corpera
                        octopus octopodes
                        deer deer
                        mouse mice
                        sword swords
                        address addresses
                        person-hour people-hours
                        woman women
                        child children
                        loaf loaves
                        knife knives
                        car cars
                        phalanx phalanges
                        larynx larynges
                        invoice invoices
                        ) by #'cddr
   do (assert (equal (make-english-singular pl) sing))
   do (assert (equal (make-english-plural sing) pl)))





(defun-lang plural (count string)
  (:en
   (if (= 1 count)
       string
       (funcall
        (letter-case string)
        (make-english-plural string))))
  (:fr (if (= 1 count)
           string
           (cond                        ; FIXME
             (t (funcall (letter-case string)
                         (strcat string "s"))))))
  (:es (if (= 1 count)
           string
           (cond                        ; FIXME
             (t (funcall (letter-case string)
                         (strcat string "s"))))))
  (:ga
   (if (= 1 count)
       string
       (funcall (letter-case string)
                (irish-plural-form string)))))

(defun-lang singular (plural-string)
  (:en (make-english-singular plural-string)))


;;; Make sure that we create correct plural forms
(defun post-irish-plurals ()
  (let ((singulars ($$$ bád fear béal íasc síol bacach taoiseach gaiscíoch
                        deireadh saol
                        beach bos scornach eaglais aisling
                        cainteoir gnólacht tincéir am
                        adhmáil beannacht ban-aba canúint droim
                        bata ciste cailín runaí rí bus
                        ordú cruinniú
                        bearna féile
                        aidiacht aiste anáil bacach bádóir
                        báicéir baincéir bainis béal buidéal caint
                        cat céad ceadúnas ceann ceart cinnúint
                        cléreach cliabh cogadh coileach coláiste
                        comhairle deis dochtúir
                        súil deoir cuibreach))
        (plurals ($$$ báid fir béil éisc síl bacaigh taoisigh gaiscigh
                      deirí saolta
                      beacha bosa scornacha eaglaisí aislingí
                      cainteorí gnólachtaí tincéirí amanna
                      admhálacha beannachtaí ban-abaí canúintí dromanna
                      bataí cistí cailíní runaithe rithe busanna
                      orduíthe cruinnithe
                      bearnaí féilte
                      aidiachta aiste anála bacaigh bádóra báiceára
                      baincéara bainise béil buidéil cainte cait céid
                      ceadúnais cinn cirt cinniúna clérigh
                      cléibh cogaidh coiligh coláiste
                      comhairle deise dochtúra
                      súila deora cubraigha)))
    (let ((computed (loop for s in singulars
                       collecting (plural% :ga 2 s))))

      (loop for s in singulars
         for pl in plurals
         for c-pl in computed
         if (equal pl c-pl)
         count 1 into good
         else do
           (warn "Failure in Irish plural: ~A ⇒ ✓ ~A (got ✗“~A”) — ~:r decl. ~A"
                 s pl c-pl (declension-of% :ga s)(gender-of% :ga s))
         finally (return (values good #1=(/ good (length singulars))
                                 (strcat (round #1# 1/100) "%")))))))
(post-irish-plurals)



(define-constant spanish-numbers
    (mapplist (key value)
        ($$$ 1 uno
             2 dos
             3 tres
             4 cuatro
             5 cinco
             6 seis
             7 siete
             8 ocho
             9 nueve
             10 diez
             11 once
             12 doce
             13 trece
             14 catorce
             15 quince
             16 dieciséis
             17 diecisiete
             18 dieciocho
             19 diecinueve
             20 veinte
             21 veintiuno
             22 veintidós
             23 veintitrés
             24 veinticuatro
             25 veinticinco
             26 veintiséis
             27 veintisiete
             28 veintiocho
             29 veintinueve
             30 treinta
             40 cuarenta
             50 cincuenta
             60 sesenta
             70 setenta
             80 ochenta
             90 noventa
             100 cien ;; ciento +
             200 doscientos
             300 trescientos
             400 cuatrocientos
             500 quinientos
             600 seiscientos
             700 setecientos
             800 ochocientos
             900 novecientos
             1000 mil)
      (list (parse-integer key) value))
  :test 'equal)

(defun-lang counting (count string)
  (:en (cond
         ((zerop count) (a/an/some% :en 0 string))
         ((< count 21) (funcall (letter-case string)
                                (format nil "~R ~A" count
                                        (plural% :en count string))))
         (t (format nil "~:D ~A" count (plural% :en count string)))))
  (:es (cond
         ((zerop count) (a/an/some 0 string))
         ((= 1 count) (funcall (letter-case string)
                               (strcat (ecase (gender-of% :es string)
                                         (:m "un ")
                                         (:f "una "))
                                       string)))
         ((< count 31) (funcall (letter-case string)
                                (strcat (getf spanish-numbers count)
                                        " "
                                        (plural% :es count string))))
         (t (format nil "~,,'.:D ~A" count (plural% :es count string)))))
  (:la (cond
         ((zerop count) (a/an/some 0 string))
         ((= 1 count) (funcall (letter-case string)
                               (strcat (ecase (gender-of% :la string)
                                         (:m "unus ")
                                         (:f "una ")
                                         (:n "unum "))
                                       string)))
         ((< count 11) (funcall (letter-case string)
                                (strcat (getf '(1 nil
                                                2 "duō"
                                                3 "trēs"
                                                4 "quatuor"
                                                5 "quinque"
                                                6 "sex"
                                                7 "septem"
                                                8 "octem"
                                                9 "novem"
                                                10 "decem"
                                                ) count)
                                        " "
                                        (plural% :la count string))))
         ((< count 5000) (presentation-roman-numeral (format nil "~:@r ~A" count (plural% :es count string))))
         (t (format nil "~,,'.:D ~A" count (plural% :es count string))))))

(assert (equal (counting% :es 2 "gato") "dos gatos"))
(assert (equal (counting% :es 1492 "gato") "1.492 gatos"))
(assert (equal (counting% :es 1 "gato") "un gato"))
(assert (equal (counting% :es 1 "casa") "una casa"))

(defun-lang a/an (string)
  (:la string)
  (:en (let ((letter (elt string 0)))
         (case letter
           ((#\a #\e #\i #\o #\u #\h)
            (concatenate 'string "an " string))
           ((#\A #\E #\I #\O #\U #\H)
            (concatenate 'string (funcall (letter-case string) "an ") string))
           (otherwise
            (concatenate 'string (funcall (letter-case string) "a ") string)))))
  (:es (strcat (funcall (letter-case string)
                        (ecase (gender-of% :es string)
                          ((:m nil) "un ")
                          (:f "una "))) string))
  (:fr (strcat (funcall (letter-case string)
                        (ecase (gender-of% :fr string)
                          ((:m nil) "un ")
                          (:f "une "))) string))
  (:ga string))

(defun-lang pluralp (string)
  (:en (char-equal (last-elt string) #\s))
  (:es (char-equal (last-elt string) #\s))
  (:fr (char-equal (last-elt string) #\s)))

(defun-lang -the- (string)
  (:la string)
  (:ru string)
  (:en (concatenate 'string (funcall (letter-case string) "the ") string))
  (:es (strcat (funcall (letter-case string)
                        (ecase (pluralp% :es string)
                          (t (ecase (gender-of% :es string)
                               ((:m nil) "los ")
                               (:f "las ")))
                          ((nil) (ecase (gender-of% :es string)
                                   ((:m nil) "el ")
                                   (:f "la "))))) string))
  (:fr (strcat (funcall (letter-case string)
                        (ecase (pluralp% :es string)
                          (t "les ")
                          ((nil) (ecase (gender-of% :es string)
                                   ((:m nil) (if (vowelp (first-elt string))
                                                 "l'"
                                                 "le "))
                                   (:f "la "))))) string))
  (:ga (concatenate 'string (funcall (letter-case string) "an ")
                    string)))


(defun-lang a/an/some (count string)
  (:en (case count
         (0 (concatenate 'string (funcall (letter-case string) "no ")
                         (plural% :en 0 string)))
         (1 (a/an string))
         (otherwise (concatenate 'string (funcall (letter-case string) "some ")
                                 (plural% :en count string)))))
  (:fr (case count
         (0 (concatenate 'string (funcall (letter-case string) "sans ")
                         (plural% :fr 0 string)))
         (1 (a/an string))
         (otherwise (concatenate 'string (funcall (letter-case string) "des ")
                                 (plural% :fr count string)))))
  (:ga (plural% :ga count string)))

;;; Credit for Irish language test  cases to Irish language documents by
;;; Amy    de     Buitléir,    CC-BY     3.0    license,     found    at
;;; http://unaleargais.ie/foghlaim/                                    …
;;; http://creativecommons.org/licenses/by/3.0/




;;; Human-friendly formats in and output

(defun range-size (numeric-range-string)
  "Count the length of a range of numbers separated by -"
  (if (find #\- numeric-range-string)
      (destructuring-bind (start end)
          (uiop:split-string numeric-range-string
                             :separator "-")
        (1+ (- (parse-integer end) (parse-integer start))))
      1))

(defun human-duration (seconds)
  (cond
    ((< seconds 1/1000000000000000)
     "instantly")
    ((< seconds 1/1000000000000)
     (format nil "~d femtosecond~:p" (round (* seconds 1000000000000000))))
    ((< seconds 1/1000000000)
     (format nil "~d picosecond~:p" (round (* seconds 1000000000000))))
    ((< seconds 1/1000000)
     (format nil "~d nanosecond~:p" (round (* seconds 1000000000))))
    ((< seconds 1/1000)
     (format nil "~d microsecond~:p" (round (* seconds 1000000))))
    ((< seconds 1)
     (format nil "~d millisecond~:p" (round (* seconds 1000))))
    ((< seconds 90)
     (format nil "~d second~:p" (round seconds)))
    ((< seconds (* 90 60))
     (format nil "~d minutes" (round seconds 60)))
    ((< seconds (* 3 24 60 60))
     (format nil "~d hours" (round seconds (* 60 60))))
    ((< seconds (* 6 7 24 60 60))
     (format nil "~d days" (round seconds (* 24 60 60))))
    ((< seconds (* 75 7 24 60 60))
     (format nil "~d weeks" (round seconds (* 7 24 60 60))))
    (t (format nil "~:d years" (round seconds (* 365.2489 24 60 60))))))
