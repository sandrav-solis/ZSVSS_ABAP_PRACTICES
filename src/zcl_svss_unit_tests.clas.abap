CLASS zcl_svss_unit_tests DEFINITION PUBLIC FINAL FOR TESTING

DURATION SHORT
RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS zcl_svss_unit_tests IMPLEMENTATION.
  METHOD first_test.

    DO 1000 TIMES.
      SELECT * FROM zdlc_travel_001
      WHERE travel_id = '00000022'
       INTO TABLE @DATA(it_travel).
    ENDDO.

   loop at it_travel ASSIGNING FIELD-SYMBOL(<fs_travel>).
   ENDLOOP.

  ENDMETHOD.
ENDCLASS.
