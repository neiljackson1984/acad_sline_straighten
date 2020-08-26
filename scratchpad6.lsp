;; do a "multi-stretch" horizontally and vertically to achieve desired row and column spacing 

; PARAMETERS: 
(setq preferredGridInterval 0.25)


(setq padding
    (list
        ;columnPadding:
        (* 10 preferredGridInterval)
        
        ;rowPadding:
        (* 5 preferredGridInterval)
    )
)


(setq selectionBleed 0.25) ; extra size around the nominal selection rectangle to grab, to make sure we select the danglers.


;=======================================
;=======================================

(progn ; utility



    (progn ;;utility functions
        (progn
                
                ;;; copied on 2016/01/17 from http://www.theswamp.org/index.php?topic=31674.5;wap2 

            ;;;======================== VARIANTS & SAFEARRAYS ========================;;;

            ;; Variant -> LISP

            ;; gc:VariantToLispData
            ;; Converts a variant or a safearray into LISP data (list)
            ;;
            ;; Argument: var variant or safearray

            (defun gc:VariantToLispData (var)
              (cond
                ((= (type var) 'variant)
                 (gc:VariantToLispData (vlax-variant-value var)))
                ((= (type var) 'safearray)
                 (mapcar 'gc:VariantToLispData (vlax-safearray->list var))
                )
                (T var)
              )
            )

            ;; gc:2dVariantToPointList
            ;; Converts a variant of 2D coordinates into a 2d points list
            ;; LightweightPolyline: OCS coordinates
            ;;
            ;; Argument
            ;; var: a variant (array of doubles) as returned by vla-get-Coordinates

            (defun gc:2dVariantToPointList (var / foo)
              (defun foo (lst)
                (if lst
                  (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
                )
              )
              (foo (vlax-safearray->list (vlax-variant-value var)))
            )

            ;; gc:3dVariantToPointList
            ;; Converts a variant of 3D coordinates into a 3d points list
            ;; 2d Polyline: OCS coordinates (Z = 0)
            ;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
            ;; PolygonMesh, Solid, Trace: WCS coordinates
            ;;
            ;; Argument
            ;; var: a variant (array of doubles) as returned by vla-get-Coordinates

            (defun gc:3dVariantToPointList (var / foo)
              (defun foo (lst)
                (if lst
                  (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
                )
              )
              (foo (vlax-safearray->list (vlax-variant-value var)))
            )

            ;; gc:VariantsToDxfList
            ;; Returns an assoc list (DXF list type)
            ;;
            ;; Arguments
            ;; xtyp: variant (array of integers)
            ;; xval: varinat (array of variants)

            (defun gc:VariantsToDxfList (xtyp xval)
              (mapcar 'cons (gc:VariantToLispData xtyp) (gc:VariantToLispData xval))
            )

            ;; gc:GetXdata
            ;; Returns the object xadta list
            ;;
            ;; Arguments
            ;; obj: (vla-object) the object containing xdata
            ;; app: (string) the registred application name ("" for all)

            (defun gc:GetXdata (obj app / xtyp xval)
              (vla-GetXdata obj app 'xtyp 'xval)
              (gc:VariantsToDxfList xtyp xval)
            )

            ;; gc:GetXrecordData
            ;; Returns the xrecord object DXF data list
            ;;
            ;; Arguments
            ;; xrec: (vla-object) thet XRECORD object

            (defun gc:GetXrecordData (xrec / xtyp xval)
              (vla-GetXrecordData xrec 'xtyp 'xval)
              (gc:VariantsToDxfList xtyp xval)
            )

            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

            ;; LISP -> variant

            ;; gc:2dPointListToVariant (gile)
            ;; Return a variant of 2d coordinates
            ;;
            ;; Argument: a 2d points list -type (x y)-

            (defun gc:2dPointListToVariant (lst)
              (vlax-make-variant
                (vlax-safearray-fill
                  (vlax-make-safearray
                    vlax-VbDouble
                    (cons 0 (1- (* 2 (length lst))))
                  )
                  (apply 'append lst)
                )
              )
            )

            ;; gc:3dPointListToVariant (gile)
            ;; Return a variant of 3d coordinates
            ;;
            ;; Argument: a 3d points list -type (x y z)-

            (defun gc:3dPointListToVariant (lst)
              (vlax-make-variant
                (vlax-safearray-fill
                  (vlax-make-safearray
                    vlax-VbDouble
                    (cons 0 (1- (* 3 (length lst))))
                  )
                  (apply 'append lst)
                )
              )
            )

            ;; gc:ObjectListToVariant
            ;; returns a variant (array of objects)
            ;;
            ;; Argument
            ;; lst: a vla-object list

            (defun gc:ObjectListToVariant (lst)
              (vlax-make-variant
                (vlax-safearray-fill
                  (vlax-make-safearray
                    vlax-vbObject
                    (cons 0 (1- (length lst)))
                  )
                  lst
                )
              )
            )

            ;; gc:DxfListToVariants
            ;; Defines 2 variables and bounds a variant to each
            ;;
            ;; Arguments
            ;; lst: a DXF list
            ;; typeSymbol: a quoted symbol (other than 'typeSymbol)
            ;; valueSymbol: a quoted symbol (other than 'valueSymbol)

            (defun gc:DxfListToVariants (lst typeSymbol valueSymbol)
              (set typeSymbol
                   (vlax-make-variant
                     (vlax-safearray-fill
                       (vlax-make-safearray
                         vlax-vbInteger
                         (cons 0 (1- (length lst)))
                       )
                       (mapcar 'car lst)
                     )
                   )
              )
              (set valueSymbol
                   (vlax-make-variant
                     (vlax-safearray-fill
                       (vlax-make-safearray
                         vlax-vbVariant
                         (cons 0 (1- (length lst)))
                       )
                       (mapcar '(lambda (x)
                                  (if (listp (setq x (cdr x)))
                                    (vlax-3d-point x)
                                    (vlax-make-variant x)
                                  )
                                )
                               lst
                       )
                     )
                   )
              )
            )


            ;; gc:SetXdata
            ;; Set xdatas to an object
            ;;
            ;; Arguments
            ;; obj: (vla-object) the object to set xdatas
            ;; lst: (liste DXF) the xdatas as:
            ;; '((1001 . "App_Name") (1002 . "{") (1000 . "string") (1070 . 1) (1002 . "}"))

            (defun gc:SetXdata (obj lst / xtyp xval)
              (gc:DxfListToVariants lst 'xtyp 'xval)
              (vla-SetXdata obj xtyp xval)
            )

            ;; gc:SetXrecordData
            ;; Set datas to an xrecord
            ;;
            ;; Arguments
            ;; xrec: (vla-object) the Xrecord object
            ;; lst : (liste DXF) the datas as:
            ;; '((1 . "string") (70 . 1) (10 1.0 2.0 0.0))

            (defun gc:SetXrecordData (xrec lst / xtyp xval)
              (gc:DxfListToVariants lst 'xtyp 'xval)
              (vla-SetXrecordData xrec xtyp xval)
            )

        
        )
        
        
        
        ;; gc:2dVariantToPointList
        ;; Converts a variant of 2D coordinates into a 2d points list
        ;; LightweightPolyline: OCS coordinates
        ;;
        ;; Argument
        ;; var: a variant (array of doubles) as returned by vla-get-Coordinates

        (defun gc:2dVariantToPointList (var / foo)
          (defun foo (lst)
            (if lst
              (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
            )
          )
          (foo (vlax-safearray->list (vlax-variant-value var)))
        )

        ;


        ;; gc:3dVariantToPointList
        ;; Converts a variant of 3D coordinates into a 3d points list
        ;; 2d Polyline: OCS coordinates (Z = 0)
        ;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
        ;; PolygonMesh, Solid, Trace: WCS coordinates
        ;;
        ;; Argument
        ;; var: a variant (array of doubles) as returned by vla-get-Coordinates

        (defun gc:3dVariantToPointList (var / foo)
          (defun foo (lst)
            (if lst
              (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
            )
          )
          (foo (vlax-safearray->list (vlax-variant-value var)))
        )
        ;========

        ;;----------------=={ 3D Rotate by Matrix }==-----------------;;
        ;;                                                            ;;
        ;;  Rotates a VLA-Object or Point List about a 3D axis using  ;;
        ;;  a Transformation matrix.                                  ;;
        ;;------------------------------------------------------------;;
        ;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
        ;;------------------------------------------------------------;;
        ;;  Arguments:                                                ;;
        ;;  target - VLA-Object or Point List to Rotate               ;;
        ;;  p1,p2  - Two 3D points defining the axis of rotation      ;;
        ;;  ang    - Rotation Angle                                   ;;
        ;;------------------------------------------------------------;;
        (defun LM:Rotate3D ( target p1 p2 ang / ux uy uz )

          (mapcar 'set '(ux uy uz) (setq u (unit (mapcar '- p2 p1))))

          (LM:ApplyMatrixTransformation target
            (setq m
              (m+m
                (list
                  (list (cos ang) 0. 0.)
                  (list 0. (cos ang) 0.)
                  (list 0. 0. (cos ang))
                )
                (m+m
                  (mxs
                    (list
                      (list 0. (- uz) uy)
                      (list uz 0. (- ux))
                      (list (- uy) ux 0.)
                    )
                    (sin ang)
                  )
                  (mxs (mapcar '(lambda ( e ) (vxs u e)) u) (- 1. (cos ang)))
                )
              )
            )      
            (mapcar '- p1 (mxv m p1))
          )
        )
        ;====================

        ;;----------------=={ 3D Reflect by Matrix }==----------------;;
        ;;                                                            ;;
        ;;  Reflects a VLA-Object or Point List in a plane using a    ;;
        ;;  Transformation matrix.                                    ;;
        ;;------------------------------------------------------------;;
        ;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
        ;;------------------------------------------------------------;;
        ;;  Arguments:                                                ;;
        ;;  target   - VLA-Object or Point List to Reflect            ;;
        ;;  p1,p2,p3 - Three 3D points defining the reflection plane  ;;
        ;;------------------------------------------------------------;;

        (defun LM:Reflect3D ( target p1 p2 p3 / m u ux uy uz )

          (mapcar 'set '(ux uy uz) (setq u (unit (v^v (mapcar '- p2 p1) (mapcar '- p3 p1)))))

          (LM:ApplyMatrixTransformation target
            (setq m
              (list
                (list (- 1. (* 2. ux ux)) (* -2. uy ux) (* -2. ux uz))
                (list (* -2. ux uy) (- 1. (* 2. uy uy)) (* -2. uy uz))
                (list (* -2. ux uz) (* -2. uy uz) (- 1. (* 2. uz uz)))
              )
            )
            (mapcar '- p1 (mxv m p1))
          )
        )

        ;;-----------=={ Apply Matrix Transformation }==--------------;;
        ;;                                                            ;;
        ;;  Transforms a VLA-Object or Point List using a             ;;
        ;;  Transformation Matrix                                     ;;
        ;;------------------------------------------------------------;;
        ;;  Author: Lee Mac, Copyright © 2010 - www.lee-mac.com       ;;
        ;;------------------------------------------------------------;;
        ;;  Arguments:                                                ;;
        ;;  target - VLA-Object or Point List to Transform            ;;
        ;;  matrix - 3x3 Matrix by which to Transform object          ;;
        ;;  vector - 3D translation vector                            ;;
        ;;------------------------------------------------------------;;

        (defun LM:ApplyMatrixTransformation ( target matrix vector ) (vl-load-com)
          (cond
            ( (eq 'VLA-OBJECT (type target))
             
              (vla-TransformBy target
                (vlax-tMatrix
                  (append (mapcar '(lambda ( x v ) (append x (list v))) matrix vector)
                   '((0. 0. 0. 1.))
                  )
                )
              )
            )
            ( (listp target)

              (mapcar
                (function
                  (lambda ( point ) (mapcar '+ (mxv matrix point) vector))
                )
                target
              )
            )        
          )
        )

        ;; Matrix x Vector - Vladimir Nesterovsky
        ;; Args: m - nxn matrix, v - vector in R^n

        (defun mxv ( m v )
          (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
        )

        ;; Matrix x Scalar - Lee Mac
        ;; Args: m - nxn matrix, n - real scalar

        (defun mxs ( m s )
          (mapcar '(lambda ( r ) (mapcar '(lambda ( n ) (* n s)) r)) m)
        )

        ;; Matrix + Matrix - Lee Mac
        ;; Args: m,n - nxn matrices

        (defun m+m ( m n )
          (mapcar '(lambda ( r s ) (mapcar '+ r s)) m n)
        )

        ;; Vector Norm - Lee Mac
        ;; Args: v - vector in R^n

        (defun norm ( v )
          (sqrt (apply '+ (mapcar '* v v)))
        )

        ;; Vector x Scalar - Lee Mac
        ;; Args: v - vector in R^n, s - real scalar

        (defun vxs ( v s )
          (mapcar '(lambda ( n ) (* n s)) v)
        )

        ;; Unit Vector - Lee Mac
        ;; Args: v - vector in R^n

        (defun unit ( v )
          ( (lambda ( n ) (if (equal 0.0 n 1e-14) nil (vxs v (/ 1.0 n)))) (norm v))
        )

        ;; Vector Cross Product - Lee Mac
        ;; Args: u,v - vectors in R^3

        (defun v^v ( u v )
          (list
            (- (* (cadr u) (caddr v)) (* (cadr v) (caddr u)))
            (- (* (car  v) (caddr u)) (* (car  u) (caddr v)))
            (- (* (car  u) (cadr  v)) (* (car  v) (cadr  u)))
          )
        )
        ;====
        
        
        (progn  ;; PrintListV1-0.lsp
            ;; Print List  -  Lee Mac
            ;; Prints a supplied list to the command-line or to a given filename,
            ;; with nested lists displayed in a hierarchical format.
            ;; l - [lst] List to print
            ;; f - [str] Optional filename

            (defun LM:princl ( l f / _print _princ d r )
                
                (defun _print ( l i )
                    (if (and (= 'list (type l)) (vl-list-length l) (vl-some 'vl-consp l))
                        (progn
                            (_princ (strcat "\n" i "("))
                            (foreach x l (_print x (strcat i "    ")))
                            (_princ (strcat "\n" i ")"))
                        )
                        (_princ (strcat "\n" i (vl-prin1-to-string l)))
                    )
                )

                (eval
                    (list 'defun '_princ '( x )
                        (if (and (= 'str (type f)) (setq d (open f "w")))
                            (list 'princ 'x d)
                           '(princ x)
                        )
                    )
                )

                (setq r (vl-catch-all-apply '_print (list l "")))
                (if (= 'file (type d))
                    (progn
                        (setq d (close d))
                        (startapp "notepad" f)
                    )
                )
                (if (vl-catch-all-error-p r)
                    (prompt (vl-catch-all-error-message r))
                    l
                )
            )

            (defun princl ( l ) (LM:princl l nil) (princ))
            (defun princf ( l ) (LM:princl l (vl-filename-mktemp "list" (getvar 'dwgprefix) ".txt")) (princ))
            (princ)    
        
        )
        
        ;===

        ;; rtos wrapper  -  Lee Mac
        ;; A wrapper for the rtos function to negate the effect of DIMZIN

        (defun LM:rtos ( real units prec / dimzin result )
            (setq dimzin (getvar 'dimzin))
            (setvar 'dimzin 0)
            (setq result (vl-catch-all-apply 'rtos (list real units prec)))
            (setvar 'dimzin dimzin)
            (if (not (vl-catch-all-error-p result))
                result
            )
        )




        (progn ;; DrawOrderV1-2.lsp
            ;; Move to Top  -  Lee Mac
            ;; Moves a set of objects to the top of the draw order.
            ;; obs - [lst/sel] Selection set or list of objects with same owner
            ;; Returns: T if successful, else nil

            (defun LM:movetotop ( obs / tab )
                (if (and (or (= 'list (type obs)) (setq obs (LM:ss->vla obs)))
                         (setq tab (LM:sortentstable (LM:getowner (car obs))))
                    )
                    (not (vla-movetotop tab (LM:safearrayvariant vlax-vbobject obs)))
                )
            )

            ;; Move to Bottom  -  Lee Mac
            ;; Moves a set of objects to the bottom of the draw order.
            ;; obs - [lst/sel] Selection set or list of objects with same owner
            ;; Returns: T if successful, else nil

            (defun LM:movetobottom ( obs / tab )
                (if (and (or (= 'list (type obs)) (setq obs (LM:ss->vla obs)))
                         (setq tab (LM:sortentstable (LM:getowner (car obs))))
                    )
                    (not (vla-movetobottom tab (LM:safearrayvariant vlax-vbobject obs)))
                )
            )

            ;; Move Above  -  Lee Mac
            ;; Moves a set of objects above a supplied object in the draw order.
            ;; obs - [lst/sel] Selection set or list of objects with same owner
            ;; obj - [vla] Object above which to move supplied objects
            ;; Returns: T if successful, else nil

            (defun LM:moveabove ( obs obj / tab )
                (if (and (or (= 'list (type obs)) (setq obs (LM:ss->vla obs)))
                         (setq tab (LM:sortentstable (LM:getowner (car obs))))
                    )
                    (not (vla-moveabove tab (LM:safearrayvariant vlax-vbobject obs) obj))
                )
            )

            ;; Move Below  -  Lee Mac
            ;; Moves a set of objects below a supplied object in the draw order.
            ;; obs - [lst/sel] Selection set or list of objects with same owner
            ;; obj - [vla] Object below which to move supplied objects
            ;; Returns: T if successful, else nil

            (defun LM:movebelow ( obs obj / tab )
                (if (and (or (= 'list (type obs)) (setq obs (LM:ss->vla obs)))
                         (setq tab (LM:sortentstable (LM:getowner (car obs))))
                    )
                    (not (vla-movebelow tab (LM:safearrayvariant vlax-vbobject obs) obj))
                )
            )

            ;; Swap Order  -  Lee Mac
            ;; Swaps the draw order of two objects (may require regen).
            ;; ob1,ob2 - [vla] Objects to swap
            ;; Returns: T if successful, else nil

            (defun LM:swaporder ( ob1 ob2 / tab )
                (if (setq tab (LM:sortentstable (LM:getowner ob1)))
                    (not (vla-swaporder tab ob1 ob2))
                )
            )

            ;; Get Owner -  Lee Mac
            ;; A wrapper for the objectidtoobject method & ownerid property to enable
            ;; compatibility with 32-bit & 64-bit systems

            (defun LM:getowner ( obj )
                (eval
                    (list 'defun 'LM:getowner '( obj )
                        (if (vlax-method-applicable-p obj 'ownerid32)
                            (list 'vla-objectidtoobject32 (LM:acdoc) '(vla-get-ownerid32 obj))
                            (list 'vla-objectidtoobject   (LM:acdoc) '(vla-get-ownerid   obj))
                        )
                    )
                )
                (LM:getowner obj)
            )

            ;; Catch Apply  -  Lee Mac
            ;; Applies a function to a list of parameters and catches any exceptions.
             
            (defun LM:catchapply ( fnc prm / rtn )
                (if (not (vl-catch-all-error-p (setq rtn (vl-catch-all-apply fnc prm))))
                    rtn
                )
            )

            ;; Sortents Table  -  Lee Mac
            ;; Retrieves the Sortents Table object.
            ;; obj - [vla] Block Container Object

            (defun LM:sortentstable ( obj / dic )
                (cond
                    (   (LM:catchapply 'vla-item (list (setq dic (vla-getextensiondictionary obj)) "acad_sortents")))
                    (   (LM:catchapply 'vla-addobject  (list dic "acad_sortents" "AcDbSortentsTable")))
                )
            )

            ;; Selection Set to VLA Objects  -  Lee Mac
            ;; Converts a Selection Set to a list of VLA Objects
            ;; sel - [sel] Selection set (pickset)

            (defun LM:ss->vla ( sel / idx lst )
                (if (= 'pickset (type sel))
                    (repeat (setq idx (sslength sel))
                        (setq lst (cons (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))) lst))
                    )
                )
            )

            ;; Safearray Variant  -  Lee Mac
            ;; Returns a populated safearray variant of a specified data type
            ;; typ - [int] Variant type enum (e.g. vlax-vbdouble)
            ;; lst - [lst] List of static type data

            (defun LM:safearrayvariant ( typ lst )
                (vlax-make-variant
                    (vlax-safearray-fill
                        (vlax-make-safearray typ (cons 0 (1- (length lst))))
                        lst
                    )
                )
            )

            ;; Active Document  -  Lee Mac
            ;; Returns the VLA Active Document Object

            (defun LM:acdoc nil
                (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
                (LM:acdoc)
            )

            (vl-load-com) (princ)            
        )
        
        (defun getOwner (entity)
            (vla-ObjectIDToObject (vla-get-Document entity) (vla-get-OwnerID entity))
        )
        
        (defun copyTo ( entity newOwner / 
            result
            )
            (setq result
                (vla-CopyObjects 
                    (vla-get-Document entity) 					; the database whose "CopyObjects" method we are calling (this is the database from which we are copying things)
                    (gc:ObjectListToVariant (list entity))		; the list of objects to be copied
                    newOwner ; the owner to whom thses objects will be copied					
                )
            )
            (car (gc:VariantToLispData result))
        )
        
        (defun clone ( entity / 
            )
            (copyTo entity (getOwner entity))
        )
        ;; Unique  -  Lee Mac
        ;; Returns a list with duplicate elements removed.
        (defun LM:Unique ( l )
            (if l (cons (car l) (LM:Unique (vl-remove (car l) (cdr l)))))
        )
        ;==============

        (setq regappName "neil_autogenerated_strips_2c4e194615d444bcb586f3274cd327b9") 
        (regapp regappName)    
        ;; returns T iff. the argument has some (any) data defined for our regapp, else returns nil
        (defun objectIsFlagged (arg /
                theEname
                theEntity
                returnValue
                newXData
            )
            ;=====
            (setq theEname 
                (handent (vla-get-Handle arg))
            )
            (setq theEntity 
                (entget theEname (list regappName))
            )
            (not (not (assoc -3 theEntity)))
        )

        ;; attaches some arbitrary data to the argument associated our regapp.
        ;; this action renders the object "flagged" in the sense that subsequent calls to (objectIsFlagged arg) will return T.
        (defun flagObject (arg /
            theEname
            theEntity
            xDataGroupCodeForInteger
            dummy
            dummyIntegerValueToAdd
            )
            (setq xDataGroupCodeForInteger 1070)
            (setq dummyIntegerValue 1234)
            
            (setq newXData                       
                (list 
                    -3 
                    (append 
                        (list regappName)
                        (mapcar '(lambda (x) (cons xDataGroupCodeForInteger x)) (list dummyIntegerValue))
                        ; (list
                            ; (cons xDataGroupCodeForInteger  77)
                            ; (cons xDataGroupCodeForInteger  88)
                            ; (cons xDataGroupCodeForInteger  99)
                        ; )		
                    )
                )                               
            ) 
            (setq theEname 
                (handent (vla-get-Handle arg))
            )
            (setq theEntity 
                (entget theEname (list regappName))
            )
            (if (assoc -3 theEntity)
                (progn
                    ; in this case, theEntity already has some xdata, so we will replace the existing xData with
                    ; the new xData
                                
                    (setq theEntity
                      (subst 
                        newXData 			;replacement
                        (assoc -3 theEntity) 	;needle
                        theEntity 				;haystack
                      )
                    ) 
                )
                (progn
                    ;in this case, theEntity does not have any existing xData, so we will simply append the 
                    ; new xData
                    ;(princ "there is no existing xData")(princ "\n.\n")
                    (setq theEntity
                      (cons newXData theEntity)
                    ) 
                )
            )
            ; Write the newly modified entity to the database.
            (entmod theEntity)
            T
        )

        (defun unflagObject (x /
            )
            ;; not yet implemented
            (princ)
        )

        (defun NewAcCmColor ( /
            )
            (vla-getinterfaceobject (vlax-get-acad-object) (strcat "autocad.accmcolor." (substr (getvar 'acadver) 1 2)))
        )
        (defun getAllAnnotativeScaleNames ( /
            annotativeScalesDictionary
            annotativeScale
            nameOfAnnotativeScale
            returnValue
            )

            (setq annotativeScalesDictionary 
                (vla-Item (vla-get-Dictionaries (vla-get-ActiveDocument (vlax-get-acad-object))) "ACAD_SCALELIST")
            )
            (setq returnValue nil)
            (vlax-for annotativeScale annotativeScalesDictionary
                (setq nameOfAnnotativeScale
                    (cdr (assoc 300 (entget (vlax-vla-object->ename annotativeScale))))
                )
                ; (princ "nameOfAnnotativeScale: " )(princ nameOfAnnotativeScale)(princ "\n")
                (setq returnValue
                    (append
                        returnValue
                        (list nameOfAnnotativeScale)
                    )
                )
            )
            returnValue
        )

        (progn ; dictionary functions
            
            
            
            ;;retrieves an item within a (possibly nested) dictionary,
            ;; in a null-safe way (returns nil if the key does not exist)
            (defun dictionaryItem
                (
                    theDictionary ;; a vla-object that is a dictionary
                    keyArg ;; can be either a list of strings or a string
                    /
                    keyPath ;; a list of keys, that form the path, through the nested dicrtionaries, that we want to get at
                    i
                    x
                    result
                )
                (setq keyPath (if (listp keyArg) keyArg (list keyArg)))
                (setq i 0)
                (setq x theDictionary)
                (while (and (< i (length keyPath)) x)
                    (setq result (vl-catch-all-apply 'vla-Item (list x (nth i keyPath))))
                    (if (vl-catch-all-error-p result)
                        (progn
                            (setq x nil)
                        )
                        (progn
                            (setq x result)
                        )
                    )
                    (setq i (+ 1 i))
                )
                x
            )
            ;========

            (defun keys ;;returns a list of the keys of a Dictionary object
                (
                    theDictionary
                    /
                    keysList
                    i
                )
                (setq keysList (list ))
                
                ;(type theDictionary)
                
                (setq i 0)
                (while (< i (vla-get-Count theDictionary))
                    (setq keysList
                        (append
                            keysList
                            (list 
                                (vla-GetName theDictionary 
                                    (vla-Item theDictionary i)
                                )
                            )
                        )
                    )		
                    (setq i (+ 1 i))
                )
                
                keysList
            )
            ;========\
        )
        ;==
        


    )
    ;====



    
    ;; generates the list (0 1 2 3 ... (rangeSize - 1))
    (defun range (rangeSize /
        i
        returnValue
        )
        (setq returnValue (list))
        (setq i 0)
        (while (< i rangeSize) 
            (setq returnValue (append returnValue (list i)))
            ;(setq i (+ 1 i))
            (plusPlus 'i)
        )
        returnValue
    )
    
    ;; generates the list (0 1 2 3 ... (rangeSize - 1))
    (defun rangeOfFloats (start stop step /
        i
        count
        returnValue
        )
        (if (not start) (setq start 0.0))
        (if (not step) (setq step 1.0))

        (setq returnValue (list))
        (setq count (RoundDown (/ (- stop start) step) 1))
        
        
        (setq i 0)
        (while (< i count) 
            (setq returnValue 
                (append returnValue 
                    (list 
                        (+ start (* step i))
                    )
                )
            )
            (setq i (+ 1 i))
        )
        returnValue
    )
    
    ;;; -------------------------------------------------------------------------------------
    ;;; Round a number down to the closest matching factor below the value
    ;;; -------------------------------------------------------------------------------------
    ;;; num : real/integer value to be rounded
    ;;; fact : real/integer value as factor to be rounded to
    ;;; Result: real/integer value rounded to the closest matching to fact
    ;;; -------------------------------------------------------------------------------------
    (defun RoundDown (num fact /)
     (setq num (- num (rem num fact)))
     (if (and (= (type fact) 'INT) (= (type num) 'REAL))
       (fix num)
       num
     )
    )
    
    (defun cartesianProductOfTwoLists (listA listB /
            returnValue
        )
        (apply 'append
            (mapcar
                '(lambda (elementFromListA)
                    (mapcar
                        '(lambda (elementFromListB)
                            (list elementFromListA elementFromListB)
                        )
                        listB
                    )
                )
                listA
            )
        )
    )





        
    (defun floatListToVariant (lst)
      (vlax-make-variant
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-VbDouble
            (cons 0 (1- (length lst)))
          )
          lst
        )
      )
    )



    (defun 3dPointListToSafeArray (lst)
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-VbDouble
            (cons 0 (1- (* 3 (length lst))))
          )
          (apply 'append lst)
        )
    )


    (defun 2dPointListToSafeArray (lst)
        (vlax-safearray-fill
          (vlax-make-safearray
            vlax-VbDouble
            (cons 0 (1- (* 2 (length lst))))
          )
          (apply 'append 
            (mapcar
                '(lambda (x) 
                    (list
                        (nth 0 x)
                        (nth 1 x)
                    )
                )
                lst
            )
          )
        )
    )






)
;===


(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
(setq modelspace (vla-get-ModelSpace doc))
(setq nameOfRowDelimiterBlockDefinition "rowDivider--ebf22f3713a948ca908d14131078e640")
(setq nameOfColumnDelimiterBlockDefinition "columnDivider-772d65dad08b470d91c420e71247e2a5")
(setq rowDelimeterBlockReferences (list ))
(setq columnDelimeterBlockReferences (list ))

(vlax-for entity modelSpace
    (if (= (vla-get-ObjectName entity) "AcDbBlockReference")
        (cond
            ((= (vla-get-Name entity) nameOfRowDelimiterBlockDefinition)
                (setq rowDelimeterBlockReferences
                    (append
                        rowDelimeterBlockReferences
                        (list entity)
                    )
                )
            )
            ((= (vla-get-Name entity) nameOfColumnDelimiterBlockDefinition)
                (setq columnDelimeterBlockReferences
                    (append
                        columnDelimeterBlockReferences
                        (list entity)
                    )
                )
            )            
        )
    )
)   

(princ (strcat "Found " (itoa (length rowDelimeterBlockReferences)) " row delimiters and " (itoa (length columnDelimeterBlockReferences)) " column delimiters." "\n"))
;=====

(if rowDelimeterBlockReferences
    (progn
        ; sort the row delimiters by increasing y coordinate
        (setq rowDelimeterBlockReferences
            (vl-sort
                rowDelimeterBlockReferences
                '(lambda (a b)
                    (apply
                        '<
                        ; the list of the x coordinates of a and b, respectively:
                        (mapcar
                            '(lambda (x) 
                                (nth 1 (gc:VariantToLispData   (vla-get-InsertionPoint x)   ))
                            )
                            (list a b)
                        )
                    )
                )
            )
        )
        ;====
        
        ; force the length of row delimiter block references to be even by throwing away trailing members as needed
        (if (/= (rem (length rowDelimeterBlockReferences) 2) 0)
            (progn
                (setq rowDelimeterBlockReferences
                    (reverse
                        (cdr
                            (reverse rowDelimeterBlockReferences)
                        )
                    )
                )
            )
        )
        ;====
        
    )
)
;===

(if columnDelimeterBlockReferences
    (progn 
        ; sort the column delimiters by increasing x coordinate
        (setq columnDelimeterBlockReferences
            (vl-sort
                columnDelimeterBlockReferences
                '(lambda (a b)
                    (apply
                        '<
                        ; the list of the x coordinates of a and b, respectively:
                        (mapcar
                            '(lambda (x) 
                                (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint x)   ))
                            )
                            (list a b)
                        )
                    )
                )
            )
        )
        ;====
        
        ; force the length of columnDelimiter block references to be even by throwing away trailing members as needed
        (if (/= (rem (length columnDelimeterBlockReferences) 2) 0)
            (progn
                (setq columnDelimeterBlockReferences
                    (reverse
                        (cdr
                            (reverse columnDelimeterBlockReferences)
                        )
                    )
                )
            )
        )
        ;====
    )
)
;===


(if (and rowDelimeterBlockReferences columnDelimeterBlockReferences)
    (progn
        ;at these point we have columnDelimeterBlockReferences and rowDelimeterBlockReferences, both guaranteed to be 
        ; nonempty, properly ordered, and have an even number of members.
        
        (if nil
            (progn   ;;; process the columns
                ; we assume that length of columnDelimeter block references is even, and that each column is a delimited by its own start delimeter and end delimiter
                ; (because we are not going to try to figure out bounding boxes, particularly where there are some danglers so it is a judgement call -- the user places the delimiters manually (but only has to do it once)
                
                ; we expect to have a delimiter before the first column and after the last column.
                ; we will iteratively grab everything to the right of the next column delimeter,
                ; stretch so that it as aligned with the current column delimiter (plus padding) 
                ; and then increment the pointer to the 'current' column delimiter
                (setq columnsCount (/ (length columnDelimeterBlockReferences) 2))
                
                (setq i 0)
                (setq accumulatedStretchVector (list 0.0 0.0 0.0))
                
                (while (< i (- columnsCount 1)) 
                    (princ ".\n")
                    (princ (strcat "now processing column " (itoa i) "\n"))
                    
                    
                    ;slightly redundant to do all of these, but I don't want to think about it, and it IS necessary to 
                    ; re-retrieve the maximum point, which may have been moved in the last stretch iteration.
                    (setq xMin (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint (car columnDelimeterBlockReferences))   )))
                    (setq xMax (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint (last columnDelimeterBlockReferences))   )))
                    (setq yMin (nth 1 (gc:VariantToLispData   (vla-get-InsertionPoint (car rowDelimeterBlockReferences))   )))
                    (setq yMax (nth 1 (gc:VariantToLispData   (vla-get-InsertionPoint (last rowDelimeterBlockReferences))   )))
                    
                    
                    
                    (setq lowDelimiter                      (nth  (* 2 i)        columnDelimeterBlockReferences)   )
                    (setq highDelimeter                     (nth  (+ (* 2 i) 1)  columnDelimeterBlockReferences)   )
                    (setq nextLowDelimeter                  (nth  (+ (* 2 i) 2)  columnDelimeterBlockReferences)   )
                    
                    
                    (setq lowDelimterCoordinate        (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint lowDelimiter)       )))
                    (setq highDelimterCoordinate       (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint highDelimeter)      )))
                    (setq nextLowDelimeterCoordinate   (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint nextLowDelimeter)   )))
                    (setq ultimateDelimeterCoordinate   (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint (last columnDelimeterBlockReferences))   )))
                    
                    (setq desiredNextLowDelimeterCoordinate (+ highDelimterCoordinate columnPadding))
                    
                    ; (princ (strcat "\t" "lowDelimterCoordinate: " (vl-prin1-to-string lowDelimterCoordinate)  "\n"))
                    ; (princ (strcat "\t" "highDelimterCoordinate: " (vl-prin1-to-string highDelimterCoordinate)  "\n"))
                    
                    (princ (strcat "\t" "accumulatedStretchVector: " (vl-prin1-to-string accumulatedStretchVector)   "\n"))
                    (princ (strcat "\t" "range of this section: " (vl-prin1-to-string lowDelimterCoordinate) " to "  (vl-prin1-to-string highDelimterCoordinate)   "\n"))
                    (princ (strcat "\t" "nextLowDelimeterCoordinate: " (vl-prin1-to-string nextLowDelimeterCoordinate)  "\n"))
                    (princ (strcat "\t" "desiredNextLowDelimeterCoordinate: " (vl-prin1-to-string desiredNextLowDelimeterCoordinate)  "\n"))
                    
                    
                    ; grab everything to the right of highDelimiter, (but not high delimiter itself) and stretch it 
                    ; from nextLowDelimeterDoordinate to desiredNextLowDelimeterCoordinate
                    
                    (progn ;compute selectionPickPoints
                        (setq nominalContentExtremePoints
                            (list
                                ; lower left:
                                (list highDelimterCoordinate yMin)

                                ; upper right:
                                (list ultimateDelimeterCoordinate yMax)
                            )
                        )            
                        (setq selectionRectangleExtremePoints
                            (list
                                ;lower left:
                                (list
                                    (+ (nth 0 (nth 0 nominalContentExtremePoints)) selectionBleed) ; we want to offset the left side of the selection rectangle a bit positive from the nominal content area so as to avoid danglers that belong to column i
                                    (- (nth 1 (nth 0 nominalContentExtremePoints)) selectionBleed)
                                )
                                
                                ;upper right:
                                (list
                                    (+ (nth 0 (nth 1 nominalContentExtremePoints)) selectionBleed)
                                    (+ (nth 1 (nth 1 nominalContentExtremePoints)) selectionBleed)
                                )
                            )
                        )
                        (setq selectionPickPoints
                            (list 
                                ;lower right of selectionRectangle
                                (list
                                    (nth 0 (nth 1 selectionRectangleExtremePoints))
                                    (nth 1 (nth 0 selectionRectangleExtremePoints)) 
                                )
                                
                                ;upper left of selectionRectangle
                                (list
                                    (nth 0 (nth 0 selectionRectangleExtremePoints))
                                    (nth 1 (nth 1 selectionRectangleExtremePoints))
                                )
                            )
                        )
                    )
                    ;=====
                    
                    
                    (setq stretchVector 
                        (list
                            (- desiredNextLowDelimeterCoordinate nextLowDelimeterCoordinate) 
                            0.0
                            0.0
                        )            
                    )
                    
                    (princ (strcat "\t" "selectionPickPoints: " (vl-prin1-to-string selectionPickPoints)  "\n"))
                    (princ (strcat "\t" "stretchVector: " (vl-prin1-to-string stretchVector)  "\n"))
                    

                    
                    ; (setq selectionSet
                        ; (ssget 
                            ; "C" ;crossing mode - critical for the stretch operation, I think 
                            ; (nth 0 selectionPickPoints)
                            ; (nth 1 selectionPickPoints)
                        ; )
                    ; )
                    (princ "checkpoint")
                    ;clear the current selection, if any
                    (sssetfirst nil (ssadd))
                    ; (command "._STRETCH" selectionSet (list 0 0 0) stretchVector)
                    (command "._STRETCH" "Crossing" (nth 0 selectionPickPoints) (nth 1 selectionPickPoints) "" (list 0 0 0) stretchVector)
                    
                    
                    ; (command "._regen") ; not necessary to regen
                    ; (command ".._RECTANGLE" (nth 0 selectionPickPoints) (nth 1 selectionPickPoints) ) 
                    
                    (setq accumulatedStretchVector  (mapcar '+ accumulatedStretchVector stretchVector))
                    (setq i (+ i 1))
                )
            )
            ;===
        )
        ;===
        
        (foreach i (list 0 1) ; do the columns (i = 0), then the rows (i = 1)
            ; the comments might talk about "columns", because I wrote this first thinking only about columns, but the code
            ; looks at i to figure out whether we are doing columns (i=0) or rows (i=1)
            

            (setq sectionTypeName (nth i (list "column" "row"))) ; for menaningful debugging messages
            
            
            (princ (strcat "Now processing the " sectionTypeName "s." "\n"))
            ;;; process the columns
            ; we assume that length of columnDelimeter block references is even, and that each column is a delimited by its own start delimeter and end delimiter
            ; (because we are not going to try to figure out bounding boxes, particularly where there are some danglers so it is a judgement call -- the user places the delimiters manually (but only has to do it once)
            
            
            
            ; we expect to have a delimiter before the first column and after the last column.
            ; we will iteratively grab everything to the right of the next column delimeter,
            ; stretch so that it as aligned with the current column delimiter (plus padding) 
            ; and then increment the pointer to the 'current' column delimiter
            
            
            (setq sectionDelimiters 
                (nth i (list columnDelimeterBlockReferences rowDelimeterBlockReferences))
            )   
            (setq sectionsCount (/ (length sectionDelimiters) 2))
            
            (setq j 0)
            (setq accumulatedStretchVector (list 0.0 0.0 0.0))
            
            (while (< j (- sectionsCount 1)) 
                (princ ".\n")
                (princ (strcat "now processing " sectionTypeName " " (itoa j) "." "\n"))
                
                
                ;slightly redundant to do all of these, but I don't want to think about it, and it IS necessary to 
                ; re-retrieve the maximum point, which was probably  moved during the last stretch iteration.
                (setq globalExtremePoints
                    (list
                        (list
                            ;xMin:
                            (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint (car columnDelimeterBlockReferences))   ))
                            
                            ;yMin:
                            (nth 1 (gc:VariantToLispData   (vla-get-InsertionPoint (car rowDelimeterBlockReferences))   ))
                        )
                        
                        (list 
                            ;xMax: 
                            (nth 0 (gc:VariantToLispData   (vla-get-InsertionPoint (last columnDelimeterBlockReferences))   ))
                            
                            ;yMax: 
                            (nth 1 (gc:VariantToLispData   (vla-get-InsertionPoint (last rowDelimeterBlockReferences))   ))
                        )
                    )
                )
                ;====
                
                (setq lowDelimiter                      (nth  (* 2 j)        sectionDelimiters)   )
                (setq highDelimeter                     (nth  (+ (* 2 j) 1)  sectionDelimiters)   )
                (setq nextLowDelimeter                  (nth  (+ (* 2 j) 2)  sectionDelimiters)   )
                
                (setq lowDelimterCoordinate        (nth i (gc:VariantToLispData   (vla-get-InsertionPoint lowDelimiter      )   )))
                (setq highDelimterCoordinate       (nth i (gc:VariantToLispData   (vla-get-InsertionPoint highDelimeter     )   )))
                (setq nextLowDelimeterCoordinate   (nth i (gc:VariantToLispData   (vla-get-InsertionPoint nextLowDelimeter  )   )))
                (setq ultimateDelimeterCoordinate  (nth i (gc:VariantToLispData   (vla-get-InsertionPoint (last  sectionDelimiters)   )   )))
                
                (setq desiredNextLowDelimeterCoordinate (+ highDelimterCoordinate (nth i padding)))

                (princ (strcat "\t" "accumulatedStretchVector: " (vl-prin1-to-string accumulatedStretchVector)   "\n"))
                (princ (strcat "\t" "range of this section: " (vl-prin1-to-string lowDelimterCoordinate) " to "  (vl-prin1-to-string highDelimterCoordinate)   "\n"))
                (princ (strcat "\t" "nextLowDelimeterCoordinate: " (vl-prin1-to-string nextLowDelimeterCoordinate)  "\n"))
                (princ (strcat "\t" "desiredNextLowDelimeterCoordinate: " (vl-prin1-to-string desiredNextLowDelimeterCoordinate)  "\n"))
                
                ; grab everything to the right of highDelimiter, (but not high delimiter itself) and stretch it 
                ; from nextLowDelimeterDoordinate to desiredNextLowDelimeterCoordinate
                
                (progn ;compute selectionPickPoints and stretchVector - this is not very elegant but I can't reason about the cleanest description of columns vs. rows right now.
                    (cond 
                        ( (= i 0) ;;in this case, we are doing columns...
                            (setq nominalContentExtremePoints
                                (list
                                    ; lower left:
                                    (list highDelimterCoordinate (nth 1 (nth 0 globalExtremePoints)))

                                    ; upper right:
                                    (list ultimateDelimeterCoordinate (nth 1 (nth 1 globalExtremePoints)))
                                )
                            )            
                            (setq selectionRectangleExtremePoints
                                (list
                                    ;lower left:
                                    (list
                                        (+ (nth 0 (nth 0 nominalContentExtremePoints)) selectionBleed) ; we want to offset the left side of the selection rectangle a bit positive from the nominal content area so as to avoid danglers that belong to column j
                                        (- (nth 1 (nth 0 nominalContentExtremePoints)) selectionBleed)
                                    )
                                    
                                    ;upper right:
                                    (list
                                        (+ (nth 0 (nth 1 nominalContentExtremePoints)) selectionBleed)
                                        (+ (nth 1 (nth 1 nominalContentExtremePoints)) selectionBleed)
                                    )
                                )
                            )
                            (setq stretchVector 
                                (list
                                    (- desiredNextLowDelimeterCoordinate nextLowDelimeterCoordinate) 
                                    0.0
                                    0
                                )            
                            )
                        )
                        ( (= i 1) ;;in this case, we are doing rows...
                            (setq nominalContentExtremePoints
                                (list
                                    ; lower left:
                                    (list (nth 0 (nth 0 globalExtremePoints))  highDelimterCoordinate )

                                    ; upper right:
                                    (list (nth 0 (nth 1 globalExtremePoints)) ultimateDelimeterCoordinate)
                                )
                            )            
                            (setq selectionRectangleExtremePoints
                                (list
                                    ;lower left:
                                    (list
                                        (- (nth 0 (nth 0 nominalContentExtremePoints)) selectionBleed) 
                                        (+ (nth 1 (nth 0 nominalContentExtremePoints)) selectionBleed) ; we want to offset the bottom side of the selection rectangle a bit positive from the nominal content area so as to avoid danglers that belong to row j
                                    )
                                    
                                    ;upper right:
                                    (list
                                        (+ (nth 0 (nth 1 nominalContentExtremePoints)) selectionBleed)
                                        (+ (nth 1 (nth 1 nominalContentExtremePoints)) selectionBleed)
                                    )
                                )
                            )
                            (setq stretchVector 
                                (list
                                    0.0
                                    (- desiredNextLowDelimeterCoordinate nextLowDelimeterCoordinate) 
                                    0
                                )            
                            )
                        )
                    )
                )
                ;=====
                
                

                
                (princ (strcat "\t" "selectionRectangleExtremePoints: " (vl-prin1-to-string selectionRectangleExtremePoints)  "\n"))
                (princ (strcat "\t" "stretchVector: " (vl-prin1-to-string stretchVector)  "\n"))
                

               (if
                    (apply 'and
                        (mapcar 
                            '(lambda (x) (equal x 0.0 0.00000001))
                            stretchVector
                        )
                    )
                    (progn
                        (princ (strcat "\t" "This " sectionTypeName " is already where we want it, therefore we wil do nothing." "\n"))
                    )
                    (progn
                        (princ (strcat "\t" "We are now stretching this " sectionTypeName "." "\n"))
                        ;clear the current selection, if any
                        (sssetfirst nil (ssadd))
                        (command "._STRETCH" "Crossing" (nth 0 selectionRectangleExtremePoints) (nth 1 selectionRectangleExtremePoints) "" (list 0 0 0) stretchVector)

                        ; (command "._regen") ; not necessary to regen
                        (setq accumulatedStretchVector  (mapcar '+ accumulatedStretchVector stretchVector))
                    )
                )
                (setq j (+ j 1))
            );
            (princ (strcat "Finished processing the " sectionTypeName "s." "\n"))
            (princ (strcat "\t" "accumulatedStretchVector: " (vl-prin1-to-string accumulatedStretchVector)   "\n"))   
        )
    )
    ;=======
)
;===


(princ)

