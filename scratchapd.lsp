

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


; (setq mySchematic (vlax-ename->vla-object (handent "E376D")))
; (setq mySchematic nil)
(if (not 
        (and 
            mySchematic
            (not (vlax-erased-p mySchematic))
        )
    )
    (progn
       (setq mySchematic (vlax-ename->vla-object (car (entsel))))
    )
)
; (vlax-dump-object mySchematic)


; (vlax-dump-object (vlax-ename->vla-object (car (entsel))))

(setq vertices 
    (mapcar
        '(lambda (x) (append x (list 0)))
        (gc:2dVariantToPointList (vla-get-Coordinates mySchematic ))
    )
)

(setq coordinates (list))
(setq i 0)
(while 
    (and
        (< i 5)
        (not    
            (vl-catch-all-error-p 
                (setq result
                    ; (vl-catch-all-apply 'vla-get-Coordinate
                        ; (list mySchematic i)
                    ; )                    
                    (vl-catch-all-apply 'vlax-get-property
                        (list mySchematic 'Coordinate i)
                    )
                )
            )
        )
    )
    (setq thisCoordinate 
        (gc:VariantToLispData  result)
    )
    (setq coordinates
        (append
            coordinates
            (list thisCoordinate)
        )
    )
    (setq i (+ i 1))
)




(setq curves (list))
(setq i 0)
(while 
    (and
        (< i 5)
        (progn
            (setq result
                ; (vl-catch-all-apply 'vla-get-Curve
                    ; (list mySchematic i)
                ; )
                (vl-catch-all-apply 'vlax-get-property
                    ; (list mySchematic 'Curve i)
                    (list mySchematic 'Curve (vlax-make-variant i vlax-vbInteger))
                )                    
            )
            
            (if  (vl-catch-all-error-p result)
                (progn
                    (princ (strcat "encountered exception while attempting to access curve " (itoa i) ": " (vl-catch-all-error-message result) "\n"))
                    nil
                )
                (progn
                    (setq thisCurve result )
                )
            )
        )
    )
    
    (setq curves
        (append
            curves
            (list thisCurve)
        )
    )
    (setq i (+ i 1))
)





(setq vertices (gc:VariantToLispData (vla-get-Coordinates mySchematic )))
(setq a (gc:VariantToLispData (vla-get-Coordinate mySchematic 1)))


; (setq connectors (gc:VariantToLispData (vla-get-Connectors mySchematic)))
; (setq connectors 

    ; (getpropertyvalue (vlax-vla-object->ename mySchematic) "Connectors")

; )
; (setq connectors
    ; (vlax-get-property mySchematic 'Connectors)
; )
(setq connectors (list))
(vlax-for connector (vlax-get-property mySchematic 'Connectors)
    ; (princ "found a connector: ")(princ connector)(princ "\n")
    (setq connectors
        (append connectors (list connector))
    )
)

(foreach connector connectors
    (vlax-dump-object connector)
)

(princ "1st connector: ")(princ (vla-item  (vlax-get-property mySchematic 'Connectors) 0))(princ "\n")

(if (> (length connectors) 2)
    (progn
        ; (vla-Delete (nth 1 connectors))
        ; (vla-Erase (nth 1 connectors))
        ; (vlax-invoke-method (nth 1 connectors) 'Erase)
        ; (vlax-invoke-method (nth 1 connectors) 'Delete)
        
    )
)
    

; (vlax-dump-object connectors)


(princ "mySchematic: ")(princ mySchematic)(princ "\n")
(princ "vertices: ")(princ vertices)(princ "\n")
(princ "coordinates: ")(princ coordinates)(princ "\n")
(princ "curves: ")(princ curves)(princ "\n")

(princ "a: ")(princ a)(princ "\n")

(princ "(length vertices): ")(princ (length vertices))(princ "\n")
(princ "(length coordinates): ")(princ (length coordinates))(princ "\n")
(princ "(length curves): ")(princ (length curves))(princ "\n")

(princ "(length a): ")(princ (length a))(princ "\n")

(setq startPoint (gc:VariantToLispData (vla-get-StartPoint mySchematic)))
(setq endPoint (gc:VariantToLispData (vla-get-EndPoint mySchematic)))
; (princ (type (vla-get-EndPoint mySchematic)))

(princ "startPoint: ")(princ startPoint)(princ "\n")
(princ "endPoint: ")(princ endPoint)(princ "\n")

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




(setq newCoordinates
    (3dPointListToSafeArray
        (list 
            startPoint 
            ; (list 3502.0 -142.0 0.0)
            endPoint
        )
    )
)



; (princ "(gc:VariantToLispData newCoordinates): ")(princ (gc:VariantToLispData newCoordinates))(princ "\n")

(setq modelSpace (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object))))
; (setq polyline (vla-AddPolyline modelSpace newCoordinates))
; (setq polyline (vla-AddPolyline modelSpace (vla-get-Coordinates mySchematic)))
(setq polyline 
    (vla-AddLightweightPolyline modelSpace 
        (2dPointListToSafeArray
            (list
                startPoint
                endPoint
            )
        ) 
    )
)


;; convert polyline to an sline, make sure the new sline matches the old sline in every relevant way, then delete the old sline.
(command "slineconvert" (vlax-vla-object->ename polyline) "" "Yes")

(setq newSchematic (vlax-ename->vla-object (entlast)))

; (command "-slinemodify" (vlax-vla-object->ename newSchematic) "" "Match" (vlax-vla-object->ename mySchematic) "All" "" )
; (command "-slinemodify" (vlax-vla-object->ename newSchematic) ""  )
; the slineconvert command wil have left the newly-created Schematic selected
(command "-slinemodify"  "Match" (vlax-vla-object->ename mySchematic) "All" ""  )
;(command "_erase" (vlax-vla-object->ename mySchematic))
(princ "mySchematic: ")(princ (vla-get-ObjectName mySchematic) )(princ "(")(princ (vla-get-Handle mySchematic))(princ ")")(princ "\n")
(princ "newSchematic: ")(princ (vla-get-ObjectName newSchematic) )(princ "(")(princ (vla-get-Handle newSchematic))(princ ")")(princ "\n")
(vla-Delete mySchematic)
; (vla-put-Coordinates polyline 
    ; (3dPointListToSafeArray
        ; (list 
            ; startPoint 
            ; ; (list 3490.0 -135.0 0.0)
            ; endPoint
        ; )
    ; )
; )

; (vla-put-Coordinates mySchematic newCoordinates)
; (vla-put-Coordinates mySchematic 

    ; (3dPointListToSafeArray
        ; (list 
            ; startPoint 
            ; ; (list 3490.0 -138.0 0.0)
            ; (list 3492.0 -147.0 0.0)
            ; endPoint
        ; )
    ; )
; )

; (setq curve (vla-Curve mySchematic))

; (dumpallproperties (vlax-vla-object->ename mySchematic))
; (princ (getpropertyvalue (vlax-vla-object->ename mySchematic) "Coordinates"))

(if nil
    (progn
        (princ "(vla-GetExtensionDictionary mySchematic): ")(princ (vla-GetExtensionDictionary mySchematic))(princ "\n")
        (princ "(keys (vla-GetExtensionDictionary mySchematic)): ")(princ (keys (vla-GetExtensionDictionary mySchematic)))(princ "\n")

        (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

        ; (vlax-for dictionary (vla-get-Dictionaries doc)
            ; (if 
                ; (not 
                    ; (vl-catch-all-error-p 
                        ; (setq name 
                            ; (vl-catch-all-apply 'vla-get-Name (list dictionary))
                        ; )
                    ; )
                ; )
                ; (progn
                
                    ; (setq theKeys (vl-catch-all-apply 'keys (list dictionary))) 
                    ; (princ "the dictionary ")(princ name) (princ " contains the folloqing keys: ")(princ theKeys)(princ "\n")
                ; )
            ; )
        ; )

        (vlax-for registeredApplication (vla-get-RegisteredApplications doc)
            (princ "registeredApplication: ") (princ (vla-get-Name registeredApplication))(princ "\n")

        )

        (setq entityData (entget (vlax-vla-object->ename mySchematic) (list "AECBASE")))
        (princ "entityData: ")(princ entityData)(princ "\n")
    )
)

(princ)


; IAecbSchematic: Schematic entity
; Property values:
;   AlternateName (RO) = "Raceway Up"
;   Application (RO) = nil
;   BoundSpaces = 1
;   Connectors (RO) = #<VLA-OBJECT IAecbConnectors 000001358510e330>
;   Coordinate = ...Indexed contents not shown...
;   Coordinates = (3486.21 -145.188 0.0 3489.94 -145.055 0.0 ... )
;   Curve = ...Indexed contents not shown...
;   Description = ""
;   Document (RO) = #<VLA-OBJECT IAcadDocument 0000012d24992898>
;   Elevation = 0.0
;   EndPoint (RO) = (3493.66 -144.923 0.0)
;   EngineeringData (RO) = #<VLA-OBJECT IAecbPartDataVariables 000001358510e5a0>
;   EngineeringID = ""
;   EntityTransparency = "ByLayer"
;   GUID (RO) = Exception occurred
;   Handle (RO) = "E37C2"
;   HasExtensionDictionary (RO) = 0
;   Hyperlinks (RO) = #<VLA-OBJECT IAcadHyperlinks 0000012d2785c838>
;   ID = " "
;   InsulationThickness = 0.0
;   Layer = "1_EQUIPMENT"
;   Linetype = "ByLayer"
;   LinetypeScale = 1.0
;   Lineweight = -1
;   LiningThickness = 0.0
;   Location = (3486.21 -145.188 0.0)
;   LockSize = 0
;   Material = "ByLayer"
;   MaximumSize = ...Indexed contents not shown...
;   Name (RO) = "Raceway Up"
;   Normal = (0.0 0.0 1.0)
;   ObjectID (RO) = 47
;   ObjectName (RO) = "AecbDbSchematic"
;   OwnerID (RO) = 43
;   PartDescription (RO) = "Raceway Up"
;   PlotStyleName = "ByLayer"
;   Rotation = 0.213189
;   ShadowDisplay = 0
;   SizeName (RO) = "Raceway Up"
;   StartPoint (RO) = (3486.21 -145.188 0.0)
;   Style = #<VLA-OBJECT IAecbSchematicStyle 00000135842ebcf0>
;   StyleName = "Raceway Up"
;   SubType (RO) = Exception occurred
;   System = #<VLA-OBJECT IAecbSchematicSystemDef 00000135842eb1f0>
;   SystemAbbreviation (RO) = ""
;   SystemFamily (RO) = ""
;   SystemLabel (RO) = Exception occurred
;   SystemName = "Standard"
;   SystemType (RO) = 0
;   TrueColor = #<VLA-OBJECT IAcadAcCmColor 0000012d2785c950>
;   Type (RO) = Exception occurred
;   Visible = -1


; IAecbSchematic: Schematic entity
; Property values:
;   AlternateName (RO) = "Raceway Up"
;   Application (RO) = nil
;   BoundSpaces = 1
;   Connectors (RO) = #<VLA-OBJECT IAecbConnectors 0000013585491ea0>
;   Coordinate = ...Indexed contents not shown...
;   Coordinates = (3484.4 -137.815 0.0 3488.13 -137.682 0.0 ... )
;   Curve = ...Indexed contents not shown...
;   Description = ""
;   Document (RO) = #<VLA-OBJECT IAcadDocument 0000012d24992898>
;   Elevation = 0.0
;   EndPoint (RO) = (3491.85 -137.549 0.0)
;   EngineeringData (RO) = #<VLA-OBJECT IAecbPartDataVariables 00000135854925f0>
;   EngineeringID = ""
;   EntityTransparency = "ByLayer"
;   GUID (RO) = Exception occurred
;   Handle (RO) = "E378E"
;   HasExtensionDictionary (RO) = 0
;   Hyperlinks (RO) = #<VLA-OBJECT IAcadHyperlinks 0000012d27841298>
;   ID = " "
;   InsulationThickness = 0.0
;   Layer = "1_EQUIPMENT"
;   Linetype = "ByLayer"
;   LinetypeScale = 1.0
;   Lineweight = -1
;   LiningThickness = 0.0
;   Location = (3484.4 -137.815 0.0)
;   LockSize = 0
;   Material = "ByLayer"
;   MaximumSize = ...Indexed contents not shown...
;   Name (RO) = "Raceway Up"
;   Normal = (0.0 0.0 1.0)
;   ObjectID (RO) = 45
;   ObjectName (RO) = "AecbDbSchematic"
;   OwnerID (RO) = 43
;   PartDescription (RO) = "Raceway Up"
;   PlotStyleName = "ByLayer"
;   Rotation = 0.213189
;   ShadowDisplay = 0
;   SizeName (RO) = "Raceway Up"
;   StartPoint (RO) = (3484.4 -137.815 0.0)
;   Style = #<VLA-OBJECT IAecbSchematicStyle 00000135842ebcf0>
;   StyleName = "Raceway Up"
;   SubType (RO) = Exception occurred
;   System = #<VLA-OBJECT IAecbSchematicSystemDef 00000135842eb1f0>
;   SystemAbbreviation (RO) = ""
;   SystemFamily (RO) = ""
;   SystemLabel (RO) = Exception occurred
;   SystemName = "Standard"
;   SystemType (RO) = 0
;   TrueColor = #<VLA-OBJECT IAcadAcCmColor 0000012d278420d0>
;   Type (RO) = Exception occurred
;   Visible = -1
; Methods supported:
;   ArrayPolar (3)
;   ArrayRectangular (6)
;   AttachAnchor (1)
;   ClosestNode (1)
;   Copy ()
;   Delete ()
;   GetAnchor ()
;   GetBoundingBox (2)
;   GetExtensionDictionary ()
;   GetXData (3)
;   Highlight (1)
;   IntersectWith (2)
;   Mirror (2)
;   Mirror3D (3)
;   Move (2)
;   NodeLocation (1)
;   ReleaseAnchor ()
;   Rotate (2)
;   Rotate3D (3)
;   ScaleEntity (2)
;   SetXData (2)
;   TransformBy (1)
;   Update ()
;

; Begin dumping object (class: AecbDbSchematic)
; AREA (type: double) = Failed to get value
; Annotative (type: bool)  (LocalName: Annotative) = 0
; AnnotativeScale (type: AcString)  (RO)  (LocalName: Annotative scale) = Failed to get value
; Area (type: double)  (RO)  (LocalName: Area) = Failed to get value
; BlockId (type: AcDbObjectId)  (RO) = 12d2640d9f0
; CastShadows (type: bool) = 1
; ClassName (type: AcString)  (RO) =
; Closed (type: bool)  (RO)  (LocalName: Closed) = Failed to get value
; CollisionType (type: AcDb::CollisionType)  (RO) = 1
; Color (type: AcCmColor)  (LocalName: Color) = BYLAYER
; EndParam (type: double)  (RO) = 9.567580
; EndPoint/X (type: double)  (RO)  (LocalName: End X) = 3493.659381
; EndPoint/Y (type: double)  (RO)  (LocalName: End Y) = -144.922595
; EndPoint/Z (type: double)  (RO)  (LocalName: End Z) = 0.000000
; ExtensionDictionary (type: AcDbObjectId)  (RO) = 0
; Handle (type: AcDbHandle)  (RO) = e37c2
; HasFields (type: bool)  (RO) = 0
; HasSaveVersionOverride (type: bool) = 0
; Hyperlinks (type: AcDbHyperlink*)
; IsA (type: AcRxClass*)  (RO) = AecbDbSchematic
; IsAProxy (type: bool)  (RO) = 0
; IsCancelling (type: bool)  (RO) = 0
; IsEraseStatusToggled (type: bool)  (RO) = 0
; IsErased (type: bool)  (RO) = 0
; IsModified (type: bool)  (RO) = 0
; IsModifiedGraphics (type: bool)  (RO) = 0
; IsModifiedXData (type: bool)  (RO) = 0
; IsNewObject (type: bool)  (RO) = 0
; IsNotifyEnabled (type: bool)  (RO) = 0
; IsNotifying (type: bool)  (RO) = 0
; IsObjectIdsInFlux (type: bool)  (RO) = 0
; IsPeriodic (type: bool)  (RO) = 0
; IsPersistent (type: bool)  (RO) = 1
; IsPlanar (type: bool)  (RO) = 1
; IsReadEnabled (type: bool)  (RO) = 1
; IsReallyClosing (type: bool)  (RO) = 1
; IsTransactionResident (type: bool)  (RO) = 0
; IsUndoing (type: bool)  (RO) = 0
; IsWriteEnabled (type: bool)  (RO) = 0
; LayerId (type: AcDbObjectId)  (LocalName: Layer) = 12d26340c10
; LineWeight (type: AcDb::LineWeight)  (LocalName: Lineweight) = -1
; LinetypeId (type: AcDbObjectId)  (LocalName: Linetype) = 12d2640d950
; LinetypeScale (type: double)  (LocalName: Linetype scale) = 1.000000
; LocalizedName (type: AcString)  (RO) =
; MaterialId (type: AcDbObjectId)  (LocalName: Material) = 12d26400e90
; MergeStyle (type: AcDb::DuplicateRecordCloning)  (RO) = 1
; ObjectId (type: AcDbObjectId)  (RO) = 13584e25a20
; OwnerId (type: AcDbObjectId)  (RO) = 12d2640d9f0
; PlotStyleName (type: AcString)  (LocalName: Plot style) = ByLayer
; ReceiveShadows (type: bool) = 1
; ShadowDisplay (type: AcDb::ShadowFlags)  (RO)  (LocalName: Shadow Display) = Failed to get value
; StartParam (type: double)  (RO) = 0.000000
; StartPoint/X (type: double)  (RO)  (LocalName: Start X) = 3485.321080
; StartPoint/Y (type: double)  (RO)  (LocalName: Start Y) = -145.232678
; StartPoint/Z (type: double)  (RO)  (LocalName: Start Z) = 0.000000
; StyleId (type: AcDbObjectId)

; IAecbConnectors: Collection of connectors
; Property values:
;   _NewEnum (RO) = #<IUnknown 0000012d299a0170>
;   Count (RO) = 3
;   Member = #<VLA-OBJECT IAecbSchematic 0000013585c93aa0>


; IAecbConnector: Connector entity
; Property values:
;   ConnectedMembers (RO) = nil
;   ConnectionType = "Undefined_Connection_Type"
;   Direction = (0.0 0.0 0.0)
;   Domain = "Schematic_Component"
;   FlowType = "FlowBidirectional"
;   Gender = "UndefinedConnectionTypeGender"
;   Height (RO) = Exception occurred
;   index = 0
;   IsPointConnector (RO) = -1
;   IsTransitioning (RO) = 0
;   Location = (3500.77 -146.793 0.0)
;   Member = #<VLA-OBJECT IAecbSchematic 0000013585c93aa0>
;   Name = ""
;   Property = ...Indexed contents not shown...
;   Property2 = ...Indexed contents not shown...
;   PropertyObject = ...Indexed contents not shown...
;   PropertyObject2 = ...Indexed contents not shown...
;   Shape (RO) = Exception occurred
;   Style = #<VLA-OBJECT IAecbConnectorStyle 0000012d29120f40>
;   System = #<VLA-OBJECT IAecbSchematicSystemDef 0000012d29120860>
;   SystemType = 0
;   WidthorDiameter (RO) = Exception occurred
; IAecbConnector: Connector entity
; Property values:
;   ConnectedMembers (RO) = nil
;   ConnectionType = "Undefined_Connection_Type"
;   Direction = (0.0 0.0 0.0)
;   Domain = "Schematic_Component"
;   FlowType = "FlowBidirectional"
;   Gender = "UndefinedConnectionTypeGender"
;   Height (RO) = Exception occurred
;   index = 1
;   IsPointConnector (RO) = -1
;   IsTransitioning (RO) = 0
;   Location = (3508.22 -146.527 0.0)
;   Member = #<VLA-OBJECT IAecbSchematic 0000013585c93aa0>
;   Name = ""
;   Property = ...Indexed contents not shown...
;   Property2 = ...Indexed contents not shown...
;   PropertyObject = ...Indexed contents not shown...
;   PropertyObject2 = ...Indexed contents not shown...
;   Shape (RO) = Exception occurred
;   Style = #<VLA-OBJECT IAecbConnectorStyle 0000012d29120f40>
;   System = #<VLA-OBJECT IAecbSchematicSystemDef 0000012d29120860>
;   SystemType = 0
;   WidthorDiameter (RO) = Exception occurred
; IAecbConnector: Connector entity
; Property values:
;   ConnectedMembers (RO) = nil
;   ConnectionType = "Undefined_Connection_Type"
;   Direction = (0.0 0.0 0.0)
;   Domain = "Schematic_Component"
;   FlowType = "FlowBidirectional"
;   Gender = "UndefinedConnectionTypeGender"
;   Height (RO) = Exception occurred
;   index = 2
;   IsPointConnector (RO) = 0
;   IsTransitioning (RO) = 0
;   Location = Exception occurred
;   Member = #<VLA-OBJECT IAecbSchematic 0000013585c93aa0>
;   Name = ""
;   Property = ...Indexed contents not shown...
;   Property2 = ...Indexed contents not shown...
;   PropertyObject = ...Indexed contents not shown...
;   PropertyObject2 = ...Indexed contents not shown...
;   Shape (RO) = Exception occurred
;   Style = #<VLA-OBJECT IAecbConnectorStyle 0000012d29120f40>
;   System = #<VLA-OBJECT IAecbSchematicSystemDef 0000012d29120860>
;   SystemType = 0
;   WidthorDiameter (RO) = Exception occurred
; 1st connector: #<VLA-OBJECT IAecbConnector 0000012d29d35540>




; Command: -PURGE
; Nested items = Off    Orphaned data = Off
; Enter type of unused objects to purge [Blocks/DEtailviewstyles/Dimstyles/Groups/LAyers/LTypes/MAterials/MUltileaderstyles/Plotstyles/SHapes/textSTyles/Mlinestyles/SEctionviewstyles/Tablestyles/Visualstyles/Regapps/Zero-length geometry/Empty text objects/Orphaned data/All]: regapps
; Enter name(s) to purge <*>: *
; Verify each name to be purged? [Yes/No] <Y>: n
; Deleting registered application "ACAD_EXEMPT_FROM_CAD_STANDARDS".
; Deleting registered application "ACAD_MLEADERVER".
; Deleting registered application "AcadAnnoAV".
; Deleting registered application "AcadAnnoPO".
; Deleting registered application "AcadAnnotativeDecomposition".
; Deleting registered application "AcadMAttdefInAnnoBlockDecomposition".
; Deleting registered application "ACAUTHENVIRON".
; Deleting registered application "AcDbAppSyncWithXref_AssocManager".
; Deleting registered application "AcDbBlockRepBTag".
; Deleting registered application "AcDbDynamicBlockTrueName".
; Deleting registered application "AcLayerTools".
; Deleting registered application "AcXRefLayerPropertyOverride".
; Deleting registered application "AdeskUnderlayLayerOverrideData".
; Deleting registered application "AEC_MEMBER_STYLE_AUTOTRIM_XDATA".
; Deleting registered application "AEC_URLDATA".
; Deleting registered application "AEC_XDATA_BOUND_SPACE".
; Deleting registered application "AECB_HALOED_LINE_OUTLINE_EDGE_LIMIT".
; Deleting registered application "AECB_OSNAP_MEP".
; Deleting registered application "AeccLand110".
; Deleting registered application "AeccLand130".
; Deleting registered application "AECGUIBASE".
; Deleting registered application "GradientColor1ACI".
; Deleting registered application "GradientColor2ACI".
; Deleting registered application "ManulPointsAssociative".
; Deleting registered application "neil_autogenerated_strips_2c4e194615d444bcb586f3274cd327b9".
; Deleting registered application "PalladioXData".
; Deleting registered application "POS_051469".
; Deleting registered application "RAK".
; Deleting registered application "RMACOMMAND_206_545-6877".
; Deleting registered application "SHEETIN2064553436".



; registeredApplication: ACAD
; registeredApplication: AECBASE
; registeredApplication: AcAecLayerStandard
; registeredApplication: ACAD_PSEXT
; registeredApplication: AEC_OBJVER_INFO
; registeredApplication: AEC_SCHEDULEDATA_FORMULA_SAMPLES
; registeredApplication: ACAUTHENVIRON
; registeredApplication: AcDbDynamicBlockTrueName
; registeredApplication: AcDbDynamicBlockGUID
; registeredApplication: AcDbBlockRepETag
; registeredApplication: AcadAnnotative
; registeredApplication: AEC_KEYNOTE_INFO
; registeredApplication: ACAD_DSTYLE_DIMJAG
; registeredApplication: ACAD_DSTYLE_DIMBREAK
; registeredApplication: ACAD_DSTYLE_DIMTALN
; registeredApplication: DCO15
; registeredApplication: AcDbBlockRepBTag
; registeredApplication: AEC_XDATA_BOUND_SPACE
; registeredApplication: AEC_SCHEDULE_TAG
; registeredApplication: SHEETIN2064553436
; registeredApplication: RMACOMMAND_206_545-6877
; registeredApplication: AcadAnnoPO
; registeredApplication: ACAD_EXEMPT_FROM_CAD_STANDARDS
; registeredApplication: ACAD_MLEADERVER
; registeredApplication: AcadAnnoAV
; registeredApplication: AcadAnnotativeDecomposition
; registeredApplication: AcadMAttdefInAnnoBlockDecomposition
; registeredApplication: AcLayerTools
; registeredApplication: AECB_HALOED_LINE_OUTLINE_EDGE_LIMIT
; registeredApplication: AECB_OSNAP_MEP
; registeredApplication: ManulPointsAssociative
; registeredApplication: ACAD_NAV_VCDISPLAY
; registeredApplication: AcCmTransparency
; registeredApplication: AeccLand110
; registeredApplication: AEC_DWGSETUP_HVAC_AIR_FLOW_UNIT
; registeredApplication: AEC_DWGSETUP_HVAC_FRICTION_UNIT
; registeredApplication: AEC_DWGSETUP_HVAC_VELOCITY_UNIT
; registeredApplication: POS_051469
; registeredApplication: GradientColor1ACI
; registeredApplication: GradientColor2ACI
; registeredApplication: AEC_MEMBER_STYLE_AUTOTRIM_XDATA
; registeredApplication: AcXRefLayerPropertyOverride
; registeredApplication: AcDbAppSyncWithXref_AssocManager
; registeredApplication: AECGUIBASE
; registeredApplication: AeccLand130
; registeredApplication: AEC_URLDATA
; registeredApplication: RAK
; registeredApplication: AdeskUnderlayLayerOverrideData
; registeredApplication: PalladioXData
; registeredApplication: neil_autogenerated_strips_2c4e194615d444bcb586f3274cd327b9