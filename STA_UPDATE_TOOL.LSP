(defun C:STA_INC (/ ss inc len i ent entList fullText findp firstLine rest coreLine
                    plusPos idx A_str B_str A B total newA newB newA_str newB_str
                    resultStr updatedText newEntList segments segCount j seg)
  (setq ss (ssget '((0 . "MTEXT"))))
  (if (null ss)
    (princ "\nNo MText selected.")
    (progn
      (setq inc (getint "\nEnter increment: "))
      (if (not inc)
        (princ "\nInvalid increment.")
        (progn
          (setq len (sslength ss) i 0)
          (while (< i len)
            ;; Get next selected MText entity
            (setq ent (ssname ss i))
            (setq entList (entget ent) fullText "")
            ;; Concatenate all group-3 and group-1 text parts
            (foreach a entList
              (cond
                ((= (car a) 3) (setq fullText (strcat fullText (cdr a))))
                ((= (car a) 1) (setq fullText (strcat fullText (cdr a))))
              )
            )
            ;; Split at first "\P"
            (setq findp (vl-string-search "\\P" fullText))
            (if findp
              (progn
                (setq firstLine (substr fullText 1 findp))
                (setq rest (substr fullText (+ findp 3))) ; Skip \P
              )
              (progn
                (setq firstLine fullText)
                (setq rest "")
              )
            )
            ;; Remove leading "STA:" if present
            (setq coreLine firstLine)
            (if (= (substr coreLine 1 4) "STA:")
              (setq coreLine (vl-string-left-trim " " (substr coreLine 5)))
            )
            ;; Find "+" and separate A and B
            (setq plusPos (vl-string-search "+" coreLine))
            (if (not plusPos)
              (princ (strcat "\nNo '+' found in MText first line: " firstLine))
              (progn
                (setq A_str (substr coreLine 1 plusPos))
                (setq B_str (substr coreLine (+ plusPos 2)))
                (setq A (atoi A_str) B (atoi B_str))
                ;; Compute new station number
                (setq total (+ (* A 100) B inc))
                (setq newA (fix (/ total 100)))
                (setq newB (rem total 100))
                ;; Format A and B
                (setq newA_str (itoa newA))
                (setq newB_str (itoa newB))
                ;; Ensure A has leading zeros if >= 3 digits
                (if (> (strlen newA_str) 2)
                  (while (< (strlen newA_str) 3)
                    (setq newA_str (strcat "0" newA_str)))
                )
                ;; Build result string
                (setq resultStr (strcat "STA: " newA_str "+" newB_str))
                ;; Rebuild full text
                (setq updatedText (strcat resultStr "\\P" rest))
                ;; Prepare modified DXF list without old text codes 1 and 3
                (setq newEntList '())
                (foreach a entList
                  (if (and (/= (car a) 1) (/= (car a) 3))
                    (setq newEntList (append newEntList (list a)))
                  )
                )
                ;; Split updatedText into 250-char segments
                (setq segments nil)
                (while (> (strlen updatedText) 250)
                  (setq segments (append segments
                                      (list (substr updatedText 1 250))))
                  (setq updatedText (substr updatedText 251))
                )
                (setq segments (append segments (list updatedText)))
                ;; Append new text segments as group 3 (all but last) and group 1 (last)
                (setq segCount (length segments) j 1)
                (foreach seg segments
                  (if (< j segCount)
                    (setq newEntList (append newEntList (list (cons 3 seg))))
                    (setq newEntList (append newEntList (list (cons 1 seg))))
                  )
                  (setq j (1+ j))
                )
                ;; Update the entity in the drawing
                (entmod newEntList)
                (entupd ent)
              ) ; end progn for plus found
            ) ; end if plusPos
            (setq i (1+ i))
          ) ; end while over selection
        ) ; end progn inc
      ) ; end if inc
    ) ; end progn ss
  ) ; end if ss
  (princ)
)