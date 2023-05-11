* Version 2 05/01/00  GLOBUS Release No. G10.2.01 25/02/00
*-----------------------------------------------------------------------------
* <Rating>895</Rating>
*-----------------------------------------------------------------------------
      SUBROUTINE ATM.PARAMETER.CROSSVAL
************************************************************************
*
*
************************************************************************
* XX/XX/XX - GBXXXXXXX
*            Pif Description
*
************************************************************************
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ATM.PARAMETER
*
************************************************************************
*
*
************************************************************************
*
      GOSUB INITIALISE
*
      GOSUB REPEAT.CHECK.FIELDS
*
      GOSUB REAL.CROSSVAL
*
      RETURN
*
************************************************************************
*
REAL.CROSSVAL:
*
* Real cross validation goes here...
*
      IF E = '' THEN
         Y.COUNT = DCOUNT(R.NEW(ATM.PARA.NETWORK.IMD),VM)
         FOR Y.CTR = 1 TO Y.COUNT
            IF R.NEW(ATM.PARA.NETWORK.IMD)<1,Y.CTR> THEN
               IF R.NEW(ATM.PARA.NET.IMD.CATEG)<1,Y.CTR>='' AND R.NEW(ATM.PARA.NET.IMD.AC.SUFX)<1,Y.CTR>='' AND R.NEW(ATM.PARA.NET.IMD.INT.AC)<1,Y.CTR>='' THEN
                  E='ENTER EITHER NET.IMD.CATEG AND NET.IMD.AC.SUFX OR NET.IMD.INT.AC'
                  AV = Y.CTR
                  AF = ATM.PARA.NET.IMD.CATEG
               END ELSE
                  IF R.NEW(ATM.PARA.NET.IMD.INT.AC)<1,Y.CTR>='' THEN
                     IF R.NEW(ATM.PARA.NET.IMD.CATEG)<1,Y.CTR> AND R.NEW(ATM.PARA.NET.IMD.AC.SUFX)<1,Y.CTR>='' THEN
                        AV = Y.CTR
                        AF = ATM.PARA.NET.IMD.AC.SUFX
                        E = 'ENTER NET.IMD.AC.SUFX'
                     END ELSE
                        IF R.NEW(ATM.PARA.NET.IMD.CATEG)<1,Y.CTR>='' AND R.NEW(ATM.PARA.NET.IMD.AC.SUFX)<1,Y.CTR> THEN
                           AV = Y.CTR
                           AF = ATM.PARA.NET.IMD.CATEG
                           E = 'ENTER NET.IMD.CATEG'
                        END
                     END
                  END
               END
            END ELSE
               IF R.NEW(ATM.PARA.NET.IMD.CATEG)<1,Y.CTR> OR R.NEW(ATM.PARA.NET.IMD.AC.SUFX)<1,Y.CTR> OR R.NEW(ATM.PARA.NET.IMD.INT.AC)<1,Y.CTR> THEN
                  E='ENTER NETWORK.IMD'
                  AV = Y.CTR
                  AF = ATM.PARA.NETWORK.IMD
               END
            END
            IF E THEN
               ETEXT = E
               CALL STORE.END.ERROR
            END
         NEXT Y.CTR
         AF = ATM.PARA.NETWORK.IMD
         ETEXT = ''
         CALL DUP
      END
      RETURN
*
************************************************************************
*
REPEAT.CHECK.FIELDS:
*
* Loop through each field and repeat the check field processing if there is any defined
*
      FOR AF = 1 TO ATM.PARA.AUDIT.DATE.TIME
         IF INDEX(N(AF), "C", 1) THEN
*
* Is it a sub value, a multi value or just a field
*
            BEGIN CASE
               CASE F(AF)[4,2] = 'XX'    ; * Sv
                  NO.OF.AV = DCOUNT(R.NEW(AF), VM)
                  IF NO.OF.AV = 0 THEN NO.OF.AV = 1
                  FOR AV = 1 TO NO.OF.AV
                     NO.OF.SV = DCOUNT(R.NEW(AF)<1,AV>, SM)
                     IF NO.OF.SV = 0 THEN NO.OF.SV = 1
                     FOR AS = 1 TO NO.OF.SV
                        GOSUB DO.CHECK.FIELD
                     NEXT AS
                  NEXT AV
               CASE F(AF)[1,2] = 'XX'    ; * Mv
                  AS = ''
                  NO.OF.AV = DCOUNT(R.NEW(AF), VM)
                  IF NO.OF.AV = 0 THEN NO.OF.AV = 1
                  FOR AV = 1 TO NO.OF.AV
                     GOSUB DO.CHECK.FIELD
                  NEXT AV
               CASE OTHERWISE
                  AV = '' ; AS = ''
                  GOSUB DO.CHECK.FIELD
            END CASE
         END
      NEXT AF
      RETURN
*
************************************************************************
*
DO.CHECK.FIELD:
** Repeat the check field validation - errors are returned in the
** variable E
*
      COMI.ENRI = ""
      BEGIN CASE
         CASE AS
            COMI = R.NEW(AF)<1,AV,AS>
         CASE AV
            COMI = R.NEW(AF)<1,AV>
         CASE AF
            COMI = R.NEW(AF)
      END CASE
*
      CALL ATM.PARAMETER.CHECK.FIELDS
      IF E THEN
         ETEXT = E
         CALL STORE.END.ERROR
      END ELSE
         BEGIN CASE
            CASE AS
               R.NEW(AF)<1,AV,AS> = COMI
               YENRI.FLD = AF:".":AV:".":AS ; YENRI = COMI.ENRI
               GOSUB SET.UP.ENRI
            CASE AV
               R.NEW(AF)<1,AV> = COMI
               YENRI.FLD = AF:".":AV ; YENRI = COMI.ENRI
               GOSUB SET.UP.ENRI
            CASE AF
               R.NEW(AF) = COMI
               YENRI.FLD = AF ; YENRI = COMI.ENRI
               GOSUB SET.UP.ENRI
         END CASE
      END
      RETURN
*
************************************************************************
*
SET.UP.ENRI:
*
      LOCATE YENRI.FLD IN T.FIELDNO<1> SETTING YPOS THEN
         T.ENRI<YPOS> = YENRI
      END
      RETURN
*
************************************************************************
*
INITIALISE:
*
      RETURN
*
************************************************************************
*
   END
