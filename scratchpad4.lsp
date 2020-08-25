(vlax-dump-object 
    (vlax-ename->vla-object (car (entsel)))
)

(vla-put-Name
    (vla-Item 
        (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
        "*U262"   
    )
    "347b8822685446829d485e09a6eea90f"
)