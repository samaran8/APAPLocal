*-----------------------------------------------------------------------------
* <Rating>69</Rating>
*-----------------------------------------------------------------------------


  SUBROUTINE REDO.GET.PROPERTY.NAME(IN.AA.ID,IN.PROPERTY.CLASS,R.OUT.AA.RECORD,OUT.PROPERTY,OUT.ERR)

* Description
*-------------
*For the Incoming arranagement ID it will return the Arrnagement record and the Property corresponds
*to the property class which was requested in the Input argument
*


*----------------------------------------------------------------------------------
*
* The routine caters to the following tasks :-
*---------------------------------------------

* Input / Output
*--------------

* IN     : IN.AA.ID,IN.PROPERTY.CLASS
* OUT    : R.OUT.AA.RECORD,OUT.PROPERTY,OUT.ER

* Dependencies
*------------

* CALLS:
*
* CALLED BY : -NA-

*-----------------------------------------------------------------------

* Revision History
* ----------------
* Date           Who                      Reference          Description
* 2-DEC-2009   H GANESH              ODR-2009-10-0346       Intial Version


$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.PROPERTY
$INSERT I_F.AA.PROPERTY.CLASS
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.TSA.STATUS
$INSERT I_TSA.COMMON
COM/AA.REDO.PROP.TYPES/AA.REDO.PROP.ID,AA.REDO.PROP.REC,AA.REDO.PROP.CLASS.REC,AA.REDO.PROP.CLASS.ID

  GOSUB INIT
  GOSUB PROCESS

  RETURN

*----------------------------------------------------------------------------------
INIT:
******
  FN.AA.ARRANGEMENT='F.AA.ARRANGEMENT'
  F.AA.ARRANGEMENT=''
  CALL OPF(FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)

  FN.AA.PROPERTY='F.AA.PROPERTY'
  F.AA.PROPERTY=''
  CALL OPF(FN.AA.PROPERTY,F.AA.PROPERTY)

  FN.AA.PROPERTY.CLASS='F.AA.PROPERTY.CLASS'
  F.AA.PROPERTY.CLASS=''
  CALL OPF(FN.AA.PROPERTY.CLASS,F.AA.PROPERTY.CLASS)

  OUT.PROPERTY=''

  RETURN
*----------------------------------------------------------------------------------
PROCESS:
*********
  CALL F.READ(FN.AA.ARRANGEMENT,IN.AA.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,ARR.ERR)
  IF R.AA.ARRANGEMENT THEN
    R.OUT.AA.RECORD=R.AA.ARRANGEMENT
    Y.PROPERTY.ARRAY=R.AA.ARRANGEMENT<AA.ARR.PROPERTY>
    LOOP
      REMOVE Y.PROPERTY FROM Y.PROPERTY.ARRAY SETTING PROP.POS
    WHILE Y.PROPERTY:PROP.POS
****PACS00523653****
            IF R.TSA.STATUS<TS.TSS.CURRENT.SERVICE>[1,23] = 'BNK/REDO.COL.FF.EXTRACT' THEN
                GET.DET = 'PROPERTY'
                GOSUB CHECK.CACHE
            END ELSE
                CALL F.READ(FN.AA.PROPERTY,Y.PROPERTY,R.PROPERTY,F.AA.PROPERTY,PROP.ERR)
            END
****PACS00523653****
      IF R.PROPERTY THEN
        Y.PROPERTY.CLASS = R.PROPERTY<AA.PROP.PROPERTY.CLASS>
        IF Y.PROPERTY.CLASS EQ IN.PROPERTY.CLASS THEN
          LOCATE Y.PROPERTY IN OUT.PROPERTY<1> SETTING POS12 ELSE
            OUT.PROPERTY<-1>=Y.PROPERTY
          END
        END
      END
    REPEAT
  END

  IF R.AA.ARRANGEMENT EQ '' THEN
    OUT.ERR='INVALID ARRANGEMNT.ID'
    RETURN
  END
****PACS00523653****
    IF R.TSA.STATUS<TS.TSS.CURRENT.SERVICE>[1,23] = 'BNK/REDO.COL.FF.EXTRACT' THEN
        GET.DET = 'PROPERTY.CLASS'
        GOSUB CHECK.CACHE
    END ELSE
        CALL F.READ(FN.AA.PROPERTY.CLASS,IN.PROPERTY.CLASS,R.PROPERTY.CLASS,F.AA.PROPERTY.CLASS,PROP.ERR)
    END
****PACS00523653****
  IF R.PROPERTY.CLASS EQ '' THEN
    OUT.ERR='INVALID PROPERTY.CLASS'
    RETURN
  END
  IF OUT.PROPERTY EQ '' THEN
    OUT.ERR='PROPERTY NOT FOUND'
  END

  RETURN
*----------------------------------------------------------------------------------
CHECK.CACHE:

    BEGIN CASE
    CASE GET.DET = 'PROPERTY'
        LOCATE Y.PROPERTY IN AA.REDO.PROP.ID<1> SETTING PROP.POS THEN
            TEMP.R.PROP.TYPE = AA.REDO.PROP.REC<PROP.POS>
            TEMP.R.PROP.TYPE = RAISE(TEMP.R.PROP.TYPE)
            R.PROPERTY = TEMP.R.PROP.TYPE
        END ELSE
            CALL F.READ(FN.AA.PROPERTY,Y.PROPERTY,R.PROPERTY,F.AA.PROPERTY,PROP.ERR)
            AA.REDO.PROP.ID<-1> = Y.PROPERTY
            TEMP.R.PROP.TYPE = R.PROPERTY
            TEMP.R.PROP.TYPE = LOWER(TEMP.R.PROP.TYPE)
            AA.REDO.PROP.REC<-1> = TEMP.R.PROP.TYPE
        END
    CASE GET.DET = 'PROPERTY.CLASS'
        LOCATE IN.PROPERTY.CLASS IN AA.REDO.PROP.CLASS.ID<1> SETTING PROP.CL.POS THEN
            TEMP.R.PROP.CLASS = AA.REDO.PROP.CLASS.REC<PROP.CL.POS>
            TEMP.R.PROP.CLASS = RAISE(TEMP.R.PROP.CLASS)
            R.PROPERTY.CLASS = TEMP.R.PROP.CLASS
        END ELSE
            CALL F.READ(FN.AA.PROPERTY.CLASS,IN.PROPERTY.CLASS,R.PROPERTY.CLASS,F.AA.PROPERTY.CLASS,PROP.ERR)
            AA.REDO.PROP.CLASS.ID<-1> = IN.PROPERTY.CLASS
            TEMP.R.PROP.CLASS = R.PROPERTY.CLASS
            TEMP.R.PROP.CLASS = LOWER(TEMP.R.PROP.CLASS)
            AA.REDO.PROP.CLASS.REC<-1> = TEMP.R.PROP.CLASS
        END
    CASE 1
    END CASE
    RETURN

END
