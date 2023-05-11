* @ValidationCode : Mjo0NTgyNjkxNjg6Q3AxMjUyOjE2ODA3ODA4Nzg3MTM6SVRTUzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 Apr 2023 17:04:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.ALE.UPD.CHEQUES.LOAD

****************************************************************
*-------------------------------------------------------------------------
* Company Name  : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By  : Arulprakasam P
* Program Name  : REDO.B.CLEAR.OUT.LOAD
*-------------------------------------------------------------------------
* Description: This routine is a load routine used to load the variables
*
*-----------------------------------------------------------------------------
* Linked with:
* In parameter :
* out parameter : None
*------------------------------------------------------------------------------
* MODIFICATION HISTORY
*------------------------------------------------------------------------------
*   DATE                ODR          Name                   DESCRIPTION
* 23-10-2011       PACS00146120      Prabhu              Initial Creation
* Date                  who                   Reference              
* 06-04-2023        �CONVERSTION TOOL   �  R22 AUTO CONVERSTION VM TO @VM
* 06-04-2023          ANIL KUMAR B         R22 MANUAL CONVERSTION -NO CHANGES
*--------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_F.REDO.APAP.CLEARING.INWARD
    $INSERT I_F.APAP.H.GARNISH.DETAILS
    $INSERT I_F.AC.LOCKED.EVENTS
    $INSERT I_F.REDO.TFS.ALE
    $INSERT I_REDO.B.ALE.UPD.CHEQUES.COMMON
    $INSERT I_F.DATES

    GOSUB INIT
    GOSUB READ.FILE

RETURN

*-----------------------------------------------------------------------------------------------------------
*****
INIT:
*****
    FN.REDO.CLEARING.OUTWARD = 'F.REDO.CLEARING.OUTWARD'
    F.REDO.CLEARING.OUTWARD = ''
    CALL OPF(FN.REDO.CLEARING.OUTWARD,F.REDO.CLEARING.OUTWARD)

    FN.APAP.H.GARNISH.DETAILS='F.APAP.H.GARNISH.DETAILS'
    F.APAP.H.GARNISH.DETAILS =''
    CALL OPF(FN.APAP.H.GARNISH.DETAILS,F.APAP.H.GARNISH.DETAILS)

    FN.REDO.TFS.ALE='F.REDO.TFS.ALE'
    F.REDO.TFS.ALE=''
    CALL OPF(FN.REDO.TFS.ALE,F.REDO.TFS.ALE)

    FN.AC.LOCKED.EVENTS='F.AC.LOCKED.EVENTS'
    F.AC.LOCKED.EVENTS=''
    CALL OPF(FN.AC.LOCKED.EVENTS,F.AC.LOCKED.EVENTS)

    FN.AC.LOCKED.EVENTS$HIS = 'F.AC.LOCKED.EVENTS$HIS'
    F.AC.LOCKED.EVENTS$HIS = ''
    CALL OPF(FN.AC.LOCKED.EVENTS$HIS,F.AC.LOCKED.EVENTS$HIS)

    FN.REDO.GAR.LOCK.ALE='F.REDO.GAR.LOCK.ALE'
    F.REDO.GAR.LOCK.ALE =''
    CALL OPF(FN.REDO.GAR.LOCK.ALE,F.REDO.GAR.LOCK.ALE)

    LOC.APPLICATION='AC.LOCKED.EVENTS'
    LOCAL.FIELD='L.AC.CUSTOMER':@VM:'L.AC.GAR.REF.NO':@VM:'L.AC.LOCKE.TYPE':@VM:'L.AC.AVAIL.BAL':@VM:'L.AC.STATUS2'

    Y.NEXT.WORK.DAY=R.DATES(EB.DAT.NEXT.WORKING.DAY)
    Y.LAST.WORK.DAY=R.DATES(EB.DAT.LAST.WORKING.DAY)

    FN.BATCH = 'F.BATCH'
    F.BATCH  = ''
    CALL OPF(FN.BATCH,F.BATCH)
    BATCH.ID = 'BNK/REDO.B.CLEAR.CHQ.ONLINE'
    Y.JOB.NAME = 'REDO.B.ALE.UPD.CHEQUES'

RETURN

*-----------------------------------------------------------------------------------------------------------
**********
READ.FILE:
**********

    CALL MULTI.GET.LOC.REF(LOC.APPLICATION,LOCAL.FIELD,LOC.POS)
    LOC.GAR.REF.POS   =LOC.POS<1,2>
    LOC.AVAIL.BAL.POS =LOC.POS<1,4>
    LOC.CUSTOMER.POS  =LOC.POS<1,1>
RETURN

*----------------------------------------------------------------------------------------------------------------
END
