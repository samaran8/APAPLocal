$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.CNV.DATE.SPANISH
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion - FM to @FM
* 06-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    GOSUB PROCESS

RETURN
*--------------
PROCESS:
*-------------

    CHECK.DATE = O.DATA
    Y.DATE = CHECK.DATE[7,2]

    Y.MONTH = CHECK.DATE[5,2]
    Y.YEAR = CHECK.DATE[1,4]

    IN.DATE = Y.DATE
    CALL REDO.CONVERT.NUM.TO.WORDS(IN.DATE, OUT.DATE, LINE.LENGTH, NO.OF.LINES, ERR.MSG)
    Y.DATE.ES = UPCASE(OUT.DATE)
    Y.DATE.ES = FIELD(Y.DATE.ES,"PESO",1)

    Y.MONTH.SPANISH = "ENERO":@FM:"FEBRERO":@FM:"MARZO":@FM:"ABRIL":@FM:"MAYO":@FM:"JUNIO":@FM:"JULIO":@FM:"AGOSTO":@FM:"SEPTIEMBRE":@FM:"OCTUBRE":@FM:"NOVIEMBRE":@FM:"DICIEMBRE"
    Y.MONTH.ES = Y.MONTH.SPANISH<Y.MONTH>

    IN.YEAR = Y.YEAR
    CALL REDO.CONVERT.NUM.TO.WORDS(IN.YEAR, OUT.YEAR, LINE.LENGTH, NO.OF.LINES, ERR.MSG)
    Y.YEAR.ES = UPCASE(OUT.YEAR)
    Y.YEAR.ES = FIELD(Y.YEAR.ES,"PESO",1)

    Y.DATE.SPANISH = Y.DATE.ES:"-":Y.DATE:"##":Y.MONTH.ES:"-":Y.MONTH:"##":Y.YEAR.ES:"-":Y.YEAR

    O.DATA = Y.DATE.SPANISH

RETURN
*--------------------------
END
