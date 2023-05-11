* @ValidationCode : MjoxNDg0MzYxMzc3OkNwMTI1MjoxNjgyNDEyMzY0NDczOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:16:04
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
	
SUBROUTINE REDO.V.VAL.SET.CODGARA

*
* Subroutine Type : ROUTINE
* Attached to     : REDO.V.VAL.SET.CODGARA
* Attached as     : ROUTINE
* Primary Purpose : COPI INFORMATION OF ID CLIENTE
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
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Pablo Castillo De La Rosa - TAM Latin America
* Date            :
*Modification history
*Date                Who               Reference                  Description
*20-04-2023      conversion tool     R22 Auto code conversion     VM TO @VM,FM TO @FM
*20-04-2023      Mohanraj R          R22 Manual code conversion   CALL method format modified
*-----------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END


RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS:
*======

    CALL APAP.REDOVER.REDO.V.VAL.ID.GARA ;* R22 Manual Conversion - CALL method format modified
    R.NEW(COLL.LOCAL.REF)<1,WPOSCOD>= COMI

RETURN
*----------------------------------------------------------------------------

INITIALISE:
*=========
    PROCESS.GOAHEAD = 1
    ZPOS = 0

    FN.COLLATERAL   = 'F.COLLATERAL'
    F.COLLATERAL    = ''
    R.COLLATERAL    = ''


    WCAMPO ="L.COL.GUAR.ID"

    WCAMPO = CHANGE(WCAMPO,@FM,@VM)
    CALL MULTI.GET.LOC.REF("COLLATERAL",WCAMPO,ZPOS)
    WPOSCOD=ZPOS<1,1>

RETURN

*------------------------
OPEN.FILES:
*=========
    CALL OPF(FN.COLLATERAL,F.COLLATERAL)
RETURN
*------------
END
