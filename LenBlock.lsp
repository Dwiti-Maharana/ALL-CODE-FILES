(defun c:LenBlock (/ ss i ent obj pt1 pt2 len roundedLen blkName midpt angle insAngle scale dx dy blkRef atts attTag)
  (vl-load-com)
  (setq scale 0.6130)

  ;; Ask for block name
  (setq blkName (getstring t "\nEnter existing block name to insert: "))
  (if (not (tblsearch "BLOCK" blkName))
    (progn (prompt (strcat "\nBlock \"" blkName "\" does not exist.")) (exit))
  )

  ;; Select multiple lines
  (prompt "\nSelect LINE objects: ")
  (setq ss (ssget '((0 . "LINE"))))
  (if (not ss)
    (progn (prompt "\nNo valid lines selected.") (exit))
  )

  ;; Process each line
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))

    ;; Get endpoints
    (setq pt1 (vlax-get obj 'StartPoint))
    (setq pt2 (vlax-get obj 'EndPoint))

    ;; Calculate length and midpoint
    (setq len (distance pt1 pt2))
    (setq roundedLen (fix (+ len 0.5)))
    (setq midpt (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))

    ;; Calculate angle manually
    (setq dx (- (car pt2) (car pt1)))
    (setq dy (- (cadr pt2) (cadr pt1)))
    (setq angle (atan dy dx))

    ;; Flip angle if text would be upside down (like dimensions)
    (setq insAngle
      (if (or (> angle (/ pi 2)) (< angle (- (/ pi 2))))
        (+ angle pi)
        angle
      )
    )

    ;; Insert block without attribute dialog
    (setq blkRef
      (vla-InsertBlock
        (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
        (vlax-3d-point midpt)
        blkName
        scale
        scale
        scale
        insAngle
      )
    )

    ;; Fill "AL" attribute with length
    (if (and blkRef (= (vla-get-HasAttributes blkRef) :vlax-true))
      (progn
        (setq atts (vlax-invoke blkRef 'GetAttributes))
        (foreach att atts
          (setq attTag (strcase (vla-get-TagString att)))
          (if (= attTag "AL")
            (vla-put-TextString att (strcat (itoa roundedLen) "'"))
          )
        )
      )
    )

    (setq i (1+ i))
  )

  (prompt (strcat "\nDone: Inserted " (itoa (sslength ss)) " block(s)."))
  (princ)
)
