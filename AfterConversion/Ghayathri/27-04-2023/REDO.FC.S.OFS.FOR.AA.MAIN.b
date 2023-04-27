* @ValidationCode : MjoxNDIyMzk5MTE1OkNwMTI1MjoxNjgyNTA5NTA4MzY1OmhhaTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 26 Apr 2023 17:15:08
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : hai
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
$PACKAGE APAP.AA
SUBROUTINE REDO.FC.S.OFS.FOR.AA.MAIN(ID.PRODUCT, STR.OFS)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE REDO.CREATE.ARRANGEMENT.AUTHORISE
* Attached as     : ROUTINE
* Primary Purpose : Build OFS for AA
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
*
*
* Error Variables:
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Juan Pablo Armas - TAM Latin America
* Date            : 01 Julio 2011
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*29-03-2023          Conversion Tool                   AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM and I++ to I=+1, I to I.VAR
*29-03-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

*-----------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.REDO.APP.MAPPING

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS.MAIN
    END

RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS.MAIN:
*=============
* Por defecto siempre se ejecuta esta parte
*
    STR.OFS = ""
    ID.APP.MAPPING = "AA.ACTIVITY"
    CALL REDO.FC.S.OFS.FOR.AA(ID.APP.MAPPING, STR.OFS)
    TEXTO := STR.OFS

* Segun el producto se ejecuta las propiedades
*
    CALL CACHE.READ(FN.REDO.APP.MAPPING, Y.IDAPP, R.REDO.APP.MAPPING, YERR)
    Y.APLICACION.FROM = R.REDO.APP.MAPPING<REDO.APP.APP.FROM>
    Y.APLICACION.TO = R.REDO.APP.MAPPING<REDO.APP.APP.TO>
    NRO.LINK = DCOUNT(R.REDO.APP.MAPPING<REDO.APP.LINK.TO.RECS>,@VM)
    II = 1
    FOR I.VAR=1 TO NRO.LINK
        STR.OFS = ""
        ID.APP.MAPPING = R.REDO.APP.MAPPING<REDO.APP.LINK.TO.RECS,I.VAR>
        Y.PROPERTY = R.REDO.APP.MAPPING<REDO.APP.ATTRIBUTE,I.VAR>
        CALL APAP.AA.REDO.FC.S.OFS.FOR.AA.PROPERTY(ID.APP.MAPPING, Y.PROPERTY, II, STR.OFS);* MANUAL R22 CODE CONVERSION
        IF STR.OFS THEN
            TEXTO :=STR.OFS
            II += 1
        END
    NEXT
    STR.OFS = TEXTO
RETURN
*------------------------
INITIALISE:
*=========

    FN.REDO.APP.MAPPING = 'F.REDO.APP.MAPPING'
    R.REDO.APP.MAPPING  = ''

    Y.IDAPP = "AA-":ID.PRODUCT
    OFS.STR.BODY  = ''
    YERR = ''
    TEXTO = ""
    PROCESS.GOAHEAD = 1
RETURN

*------------------------
OPEN.FILES:
*=========

RETURN
*------------
END
