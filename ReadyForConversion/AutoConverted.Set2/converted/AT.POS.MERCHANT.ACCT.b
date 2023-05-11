*-----------------------------------------------------------------------------
* <Rating>562</Rating>
*-----------------------------------------------------------------------------
* Version 9 15/11/00  GLOBUS Release No. G11.1.01 11/12/00

      SUBROUTINE AT.POS.MERCHANT.ACCT

******************************************************************

$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AT.POS.MERCHANT.ACCT
$INSERT I_F.ACCOUNT
$INSERT I_F.FT.COMMISSION.TYPE
*************************************************************************

      GOSUB DEFINE.PARAMETERS

      IF LEN(V$FUNCTION) GT 1 THEN
         GOTO V$EXIT
      END

      CALL MATRIX.UPDATE

      GOSUB INITIALISE                   ; * Special Initialising

*************************************************************************

* Main Program Loop

      LOOP

         CALL RECORDID.INPUT

      UNTIL (MESSAGE EQ 'RET')

         V$ERROR = ''

         IF MESSAGE EQ 'NEW FUNCTION' THEN

            GOSUB CHECK.FUNCTION         ; * Special Editing of Function

            IF V$FUNCTION EQ 'E' OR V$FUNCTION EQ 'L' THEN
               CALL FUNCTION.DISPLAY
               V$FUNCTION = ''
            END

         END ELSE

            GOSUB CHECK.ID               ; * Special Editing of ID
            IF V$ERROR THEN GOTO MAIN.REPEAT

            CALL RECORD.READ

            IF MESSAGE EQ 'REPEAT' THEN
               GOTO MAIN.REPEAT
            END

            CALL MATRIX.ALTER

            GOSUB CHECK.RECORD           ; * Special Editing of Record
            IF V$ERROR THEN GOTO MAIN.REPEAT

            LOOP
               GOSUB PROCESS.FIELDS      ; * ) For Input
               GOSUB PROCESS.MESSAGE     ; * ) Applications
            WHILE (MESSAGE EQ 'ERROR') REPEAT

         END

MAIN.REPEAT:
      REPEAT

V$EXIT:
      RETURN                             ; * From main program

*************************************************************************
*                      S u b r o u t i n e s                            *
*************************************************************************

PROCESS.FIELDS:

* Input or display the record fields

      LOOP
         IF SCREEN.MODE EQ 'MULTI' THEN
            IF FILE.TYPE EQ 'I' THEN
               CALL FIELD.MULTI.INPUT
            END ELSE
               CALL FIELD.MULTI.DISPLAY
            END
         END ELSE
            IF FILE.TYPE EQ 'I' THEN
               CALL FIELD.INPUT
            END ELSE
               CALL FIELD.DISPLAY
            END
         END

      WHILE NOT(MESSAGE)

         GOSUB CHECK.FIELDS              ; * Special Field Editing

         IF T.SEQU NE '' THEN T.SEQU<-1> = A + 1

      REPEAT

      RETURN

*************************************************************************

PROCESS.MESSAGE:

* Processing after exiting from field input (PF5)

      IF MESSAGE = 'DEFAULT' THEN
         MESSAGE = 'ERROR'               ; * Force the processing back
         IF V$FUNCTION <> 'D' AND V$FUNCTION <> 'R' THEN
            GOSUB CROSS.VALIDATION
         END
      END

      IF MESSAGE = 'PREVIEW' THEN
         MESSAGE = 'ERROR'               ; * Force the processing back
         IF V$FUNCTION <> 'D' AND V$FUNCTION <> 'R' THEN
            GOSUB CROSS.VALIDATION
            IF NOT(V$ERROR) THEN
* >               GOSUB DELIVERY.PREVIEW   ; * Activate print preview
            END
         END
      END

      IF MESSAGE EQ 'VAL' THEN
         MESSAGE = ''
         BEGIN CASE
            CASE V$FUNCTION EQ 'D'
               GOSUB CHECK.DELETE        ; * Special Deletion checks
            CASE V$FUNCTION EQ 'R'
               GOSUB CHECK.REVERSAL      ; * Special Reversal checks
            CASE OTHERWISE
               GOSUB CROSS.VALIDATION    ; * Special Cross Validation
               IF NOT(V$ERROR) THEN
                  GOSUB OVERRIDES
               END
         END CASE
         IF NOT(V$ERROR) THEN
            GOSUB BEFORE.UNAU.WRITE      ; * Special Processing before write
         END
         IF NOT(V$ERROR) THEN
            CALL UNAUTH.RECORD.WRITE
            IF MESSAGE NE "ERROR" THEN
               GOSUB AFTER.UNAU.WRITE    ; * Special Processing after write
            END
         END

      END

      IF MESSAGE EQ 'AUT' THEN
         GOSUB AUTH.CROSS.VALIDATION     ; * Special Cross Validation
         IF NOT(V$ERROR) THEN
            GOSUB BEFORE.AUTH.WRITE      ; * Special Processing before write
         END

         IF NOT(V$ERROR) THEN

            CALL AUTH.RECORD.WRITE

            IF MESSAGE NE "ERROR" THEN
               GOSUB AFTER.AUTH.WRITE    ; * Special Processing after write
            END
         END

      END

      RETURN

*************************************************************************
*                      Special Tailored Subroutines                     *
*************************************************************************

CHECK.ID:

* Validation and changes of the ID entered.  Set ERROR to 1 if in error

      IF E THEN V$ERROR = 1

      RETURN

*************************************************************************

CHECK.RECORD:

* Validation and changes of the Record.  Set ERROR to 1 if in error


      RETURN

*************************************************************************

CHECK.FIELDS:
* > CALL XX.CHECK.FIELDS
      IF E THEN
         T.SEQU = "IFLD"
         CALL ERR
      END

      RETURN

*************************************************************************

CROSS.VALIDATION:

*
      V$ERROR = ''
      ETEXT = ''
      TEXT = ''
*
* > CALL XX.CROSSVAL
*
* If END.ERROR has been set then a cross validation error has occurred
*
      IF END.ERROR THEN
         A = 1
      LOOP UNTIL T.ETEXT<A> <> "" DO A = A+1 ; REPEAT
         T.SEQU = A
         V$ERROR = 1
         MESSAGE = 'ERROR'
      END
      RETURN                             ; * Back to field input via UNAUTH.RECORD.WRITE

*************************************************************************

OVERRIDES:
*
*  Overrides should reside here
*
      V$ERROR = ''
      ETEXT = ''
      TEXT = ''
* > CALL XX.OVERRIDE
*

*
      IF TEXT = "NO" THEN                ; * Said NO to override
         V$ERROR = 1
         MESSAGE = "ERROR"               ; * Back to field input

      END
      RETURN

*************************************************************************

AUTH.CROSS.VALIDATION:


      RETURN

*************************************************************************

CHECK.DELETE:


      RETURN

*************************************************************************

CHECK.REVERSAL:


      RETURN

*************************************************************************
DELIVERY.PREVIEW:

      RETURN

*************************************************************************

BEFORE.UNAU.WRITE:
*
*  Contract processing code should reside here
*
* > CALL XX.         ;* Accounting, Schedule processing etc etc

      IF TEXT = "NO" THEN                ; * Said No to override
         CALL TRANSACTION.ABORT          ; * Cancel current transaction
         V$ERROR = 1
         MESSAGE = "ERROR"               ; * Back to field input
         RETURN
      END

*
* Additional updates should be performed here
*
* > CALL XX..



      RETURN

*************************************************************************

AFTER.UNAU.WRITE:


      RETURN

*************************************************************************

AFTER.AUTH.WRITE:


      RETURN

*************************************************************************

BEFORE.AUTH.WRITE:

      BEGIN CASE
         CASE R.NEW(V-8)[1,3] = "INA"    ; * Record status
* > CALL XX.AUTHORISATION
         CASE R.NEW(V-8)[1,3] = "RNA"    ; * Record status
* > CALL XX.REVERSAL

      END CASE

      RETURN

*************************************************************************

CHECK.FUNCTION:

* Validation of function entered.  Set FUNCTION to null if in error

      IF INDEX('V',V$FUNCTION,1) THEN
         E = 'FUNCTION NOT ALLOWED FOR THIS APPLICATION'
         CALL ERR
         V$FUNCTION = ''
      END

      RETURN

*************************************************************************

INITIALISE:

      RETURN

*************************************************************************

DEFINE.PARAMETERS:* SEE 'I_RULES' FOR DESCRIPTIONS *

      CALL AT.POS.MERCHANT.FIELD.DEFINITIONS

      RETURN

*************************************************************************

   END
