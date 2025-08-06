(defun c:ChangeBlockTextColor (/ ss i ent obj atts att tag newColor)
  (vl-load-com)

  ;; Ask user for color
  (initget 1)
  (setq newColor (getint "\nEnter desired color number (1=Red, 2=Yellow, 3=Green, 4=Cyan, 5=Blue, 6=Magenta, 7=White/Black, etc.): "))

  ;; Select blocks
  (prompt "\nSelect block(s) to change attribute color: ")
  (setq ss (ssget '((0 . "INSERT"))))
  (if (not ss)
    (progn (prompt "\nNo blocks selected.") (exit))
  )

  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))

    ;; Check if block has attributes
    (if (= (vla-get-HasAttributes obj) :vlax-true)
      (progn
        (setq atts (vlax-invoke obj 'GetAttributes))
        (foreach att atts
          ;; Change color of every attribute (or use condition to target specific tag)
          (vla-put-Color att newColor)
        )
      )
    )

    (setq i (1+ i))
  )

  (prompt "\nText color updated for selected blocks.")
  (princ)
)
