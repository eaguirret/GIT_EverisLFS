REPORT zpolytest.

CLASS lcl_animal DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: get_type ABSTRACT,
             speak ABSTRACT.
ENDCLASS.

CLASS lcl_cat DEFINITION
              INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS: get_type REDEFINITION,
             speak REDEFINITION.
ENDCLASS.

CLASS lcl_cat IMPLEMENTATION.
  METHOD get_type.
    WRITE: 'Cat'.
  ENDMETHOD.

  METHOD speak.
    WRITE: 'Meow'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_dog DEFINITION
              INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    METHODS: get_type REDEFINITION,
             speak REDEFINITION.
ENDCLASS.

CLASS lcl_dog IMPLEMENTATION.
  METHOD get_type.
    WRITE: 'Dog'.
  ENDMETHOD.

  METHOD speak.
    WRITE: 'Bark'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_see_and_say DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      play IMPORTING im_animal
                TYPE REF TO lcl_animal.
ENDCLASS.

CLASS lcl_see_and_say IMPLEMENTATION.
  METHOD play.
    WRITE: 'The'.
    CALL METHOD im_animal->get_type.
    WRITE: 'says'.
    CALL METHOD im_animal->speak.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lr_cat TYPE REF TO lcl_cat,
        lr_dog TYPE REF TO lcl_dog.

  CREATE OBJECT lr_cat.
  CREATE OBJECT lr_dog.

  CALL METHOD lcl_see_and_say=>play
    EXPORTING
      im_animal = lr_cat.
  NEW-LINE.
  CALL METHOD lcl_see_and_say=>play
    EXPORTING
      im_animal = lr_dog.