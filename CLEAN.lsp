(defun C:CLEANSTA (/ ss i ent edata txt newtxt)
  (setq ss (ssget '((0 . "MTEXT"))))
  (if (not ss)
    (prompt "\nNo MTEXT selected.")
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq edata (entget ent))
        (setq txt (cdr (assoc 1 edata)))

        ;; Remove \A1; from the beginning
        (if (wcmatch txt "\\A1;*")
          (setq newtxt (vl-string-subst "" "\\A1;" txt))
          (setq newtxt txt)
        )

        ;; Update entity
        (entmod (subst (cons 1 newtxt) (assoc 1 edata) edata))
        (entupd ent)

        (setq i (1+ i))
      )
    )
  )
  (princ "\nRemoved \\A1; if found.")
  (princ)
)
