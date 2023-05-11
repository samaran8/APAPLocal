*-----------------------------------------------------------------------------
* <Rating>-16</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE NOFILE.L.APAP.SCHEDULE.PRO.A02(Y.DATA)
*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : NOFILE.L.APAP.SCHEDULE.PRO.A02
*--------------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO               DESCRIPTION
*  20200531         ELMENDEZ          INITIAL CREATION
*-----------------------------------------------------------------------------
    $INCLUDE T24.BP I_COMMON
    $INCLUDE T24.BP I_EQUATE
    $INCLUDE BP I_F.EB.L.APAP.SCHEDULE.PROJET
    $INCLUDE T24.BP I_F.AA.PAYMENT.TYPE
    $INCLUDE T24.BP I_F.AA.PROPERTY
    $INCLUDE T24.BP I_ENQUIRY.COMMON
    $INCLUDE T24.BP I_F.USER
*-----------------------------------------------------------------------------
INIT:
    GOSUB INITILISATION
    GOSUB OPENFILES
    GOSUB SELECTCMD
    GOSUB READFILES
    GOSUB PROCESSDATA
    RETURN

INITILISATION:
    FN.SCHEDULE.PROJ = 'F.EB.L.APAP.SCHEDULE.PROJET'
    FN.SCHEDULE.PROJ<2> = 'NO.FATAL.ERROR'
    F.SCHEDULE.PROJ  = ''

    ID.SCHEDULE.PROJ = ''
    R.SCHEDULE.PROJ = ''
    SCHEDULE.PROJ.ERR = ''

    FN.PAYMENT.TYPE = 'F.AA.PAYMENT.TYPE'
    FN.PAYMENT.TYPE<2> = 'NO.FATAL.ERROR'
    F.PAYMENT.TYPE  = ''

    ID.PAYMENT.TYPE = ''
    R.PAYMENT.TYPE = ''
    PAYMENT.TYPE.ERR = ''

    FN.AA.PROPERTY = 'F.AA.PROPERTY'
    FN.AA.PROPERTY<2> = 'NO.FATAL.ERROR'
    F.AA.PROPERTY  = ''

    ID.AA.PROPERTY = ''
    R.AA.PROPERTY = ''
    AA.PROPERTY.ERR = ''

    SEL.CMD = ''
    *AA1511417411*20200415
    *AA1511417411*20200416

    Y.AA.TEMP = ''
    Y.YYYYMMDD = ''
    Y.DATA.CONCAT = ''
    Y.USR.LANGUAGE = R.USER<EB.USE.LANGUAGE>

    Y.COUNT.VM = 0
    Y.COUNT.SM = 0

        Y.SUM.PJ.TOT.CAP                   = ''
        Y.SUM.PJ.TOTAL.PRINCIPAL   = ''
        Y.SUM.PJ.TOTAL.INTEREST           = ''
        Y.SUM.PJ.TOTAL.CHARGE           = ''
    Y.SUM.PJ.TOTAL.TAX             = ''
        Y.SUM.PJ.TOTAL.PAY                   = ''    


    Y.PJ.AA.ARR.ID         = ''
    Y.PJ.ACCOUNT           = ''
    Y.PJ.YYYYMMDD          = ''
    Y.PJ.ARR.STATUS        = ''
    Y.PJ.PRODUCT.STATUS    = ''
    Y.PJ.TOT.PAYMENT       = ''
    Y.PJ.DUE.DATES         = ''
    Y.PJ.DUE.DEFER.DATES   = ''
    Y.PJ.TOT.CAP                   = ''
    Y.PJ.TOTAL.PRINCIPAL   = ''
    Y.PJ.TOTAL.INTEREST           = ''
    Y.PJ.TOTAL.CHARGE           = ''
    Y.PJ.TOTAL.TAX             = ''
    Y.PJ.TOTAL.PAY                   = ''
    Y.PJ.DUE.TYPES         = ''
    Y.PJ.DUE.METHODS       = ''
    Y.PJ.DUE.TYPE.AMTS     = ''
    Y.PJ.DUE.PROPS         = ''
    Y.PJ.DUE.PROP.AMTS     = ''
    Y.PJ.DUE.OUTS          = ''
    Y.PJ.DUE.TYPES.DESCR   = ''
    Y.PJ.DUE.PROPS.DESCR   = ''
    Y.PJ.DUE.PROPS.CLASS   = ''
    Y.PJ.TOTAL.OT.TYPE     = '' 
    Y.PJ.TOTAL.OT.PROPS    = ''
    Y.PJ.OVERRIDE          = ''
    Y.PJ.RECORD.STATUS     = ''
    Y.PJ.CURR.NO           = ''
    Y.PJ.INPUTTER          = ''
    Y.PJ.DATE.TIME         = ''
    Y.PJ.AUTHORISER        = ''
    Y.PJ.CO.CODE           = ''
    Y.PJ.DEPT.CODE         = ''
    Y.PJ.AUDITOR.CODE      = ''
    Y.PJ.AUDIT.DATE.TIME   = ''

    RETURN

OPENFILES:
    CALL OPF(FN.SCHEDULE.PROJ , F.SCHEDULE.PROJ)
    RETURN

SELECTCMD:
    *DEBUG
    
    *D.FIELDS
    *D.LOGICAL.OPERANDS
    *D.RANGE.AND.VALUES

    LOCATE "ARRANGEMENT.ID.DT" IN D.FIELDS SETTING POS THEN ID.SCHEDULE.PROJ = D.RANGE.AND.VALUE<POS> 
    
    *LIST FBNK.EB.L.APAP.SCHEDULE.PROJET 'AA1511417411*20200416'
    SEL.CMD = "SELECT FBNK.EB.L.APAP.SCHEDULE.PROJET '" : ID.SCHEDULE.PROJ : "'" 

    CALL EB.READLIST(SEL.CMD, SEL.LIST,'',NO.OF.REC,SEL.ERR)

    RETURN;

READFILES:
    CALL F.READ(FN.SCHEDULE.PROJ, ID.SCHEDULE.PROJ, R.SCHEDULE.PROJ, F.SCHEDULE.PROJ, SCHEDULE.PROJ.ERR)
    RETURN

PROCESSDATA:
    *DEBUG
    LOOP
        REMOVE AA.ARR.ID.YYYYMMDD FROM SEL.LIST SETTING PRESTAMOS
    WHILE AA.ARR.ID.YYYYMMDD:PRESTAMOS
        
        Y.AA.TEMP  = AA.ARR.ID.YYYYMMDD
        
        CHANGE '*' TO @AM IN Y.AA.TEMP

        *PROCESING VARS
        Y.PJ.AA.ARR.ID         =  Y.AA.TEMP<1> 
        Y.PJ.ACCOUNT           =  R.SCHEDULE.PROJ<EB.SP.ACCOUNT>
        Y.PJ.YYYYMMDD          =  Y.AA.TEMP<2>
        Y.PJ.ARR.STATUS        =  R.SCHEDULE.PROJ<EB.SP.ARR.STATUS>
        Y.PJ.PRODUCT.STATUS    =  R.SCHEDULE.PROJ<EB.SP.PRODUCT.STATUS>
        Y.PJ.TOT.PAYMENT       =  R.SCHEDULE.PROJ<EB.SP.TOT.PAYMENT>
        Y.PJ.DUE.DATES         =  R.SCHEDULE.PROJ<EB.SP.DUE.DATES>
        Y.PJ.DUE.DEFER.DATES   =  R.SCHEDULE.PROJ<EB.SP.DUE.DEFER.DATES>
        Y.PJ.DUE.METHODS       =  R.SCHEDULE.PROJ<EB.SP.DUE.METHODS>
        Y.PJ.DUE.TYPE.AMTS     =  R.SCHEDULE.PROJ<EB.SP.DUE.TYPE.AMTS>
        Y.PJ.DUE.PROPS         =  R.SCHEDULE.PROJ<EB.SP.DUE.PROPS>
        Y.PJ.DUE.PROP.AMTS     =  R.SCHEDULE.PROJ<EB.SP.DUE.PROP.AMTS>
        Y.PJ.DUE.OUTS          =  R.SCHEDULE.PROJ<EB.SP.DUE.OUTS>
        Y.PJ.OVERRIDE          =  R.SCHEDULE.PROJ<EB.SP.OVERRIDE>
        Y.PJ.RECORD.STATUS     =  R.SCHEDULE.PROJ<EB.SP.RECORD.STATUS>
        Y.PJ.CURR.NO           =  R.SCHEDULE.PROJ<EB.SP.CURR.NO>
        Y.PJ.INPUTTER          =  R.SCHEDULE.PROJ<EB.SP.INPUTTER>
        Y.PJ.DATE.TIME         =  R.SCHEDULE.PROJ<EB.SP.DATE.TIME>
        Y.PJ.AUTHORISER        =  R.SCHEDULE.PROJ<EB.SP.AUTHORISER>
        Y.PJ.CO.CODE           =  R.SCHEDULE.PROJ<EB.SP.CO.CODE>
        Y.PJ.DEPT.CODE         =  R.SCHEDULE.PROJ<EB.SP.DEPT.CODE>
        Y.PJ.AUDITOR.CODE      =  R.SCHEDULE.PROJ<EB.SP.AUDITOR.CODE>
        Y.PJ.AUDIT.DATE.TIME   =  R.SCHEDULE.PROJ<EB.SP.AUDIT.DATE.TIME>

        *PROCESSING FIELDS
        *DEBUG
        Y.PJ.DUE.TYPES         =  R.SCHEDULE.PROJ<EB.SP.DUE.TYPES>
        GOSUB PROCESS.TYPES
        *FILLED BY GOSUB PROCESS.TYPES
                *Y.PJ.TOT.CAP
        *Y.PJ.DUE.PROPS.DESCR 
        *Y.PJ.DUE.PROPS.CLASS '
        *Y.PJ.TOTAL.OT.TYPE

        GOSUB PROCESS.PROPS:
                *Y.PJ.TOTAL.PRINCIPAL
                *Y.PJ.TOTAL.INTEREST
                *Y.PJ.TOTAL.CHARGE
        *Y.PJ.TOTAL.TAX
                *Y.PJ.TOTAL.PAY  
        *Y.PJ.TOTAL.OT.PROPS

        **CONCAT OUTPUT
        GOSUB CONCAT
    REPEAT
    
    RETURN

CONCAT:
        *DEBUG
        Y.DATA.CONCAT = ''
        Y.DATA.CONCAT = Y.PJ.AA.ARR.ID                           
        Y.DATA.CONCAT := '*' : Y.PJ.ACCOUNT          
        Y.DATA.CONCAT := '*' : Y.PJ.YYYYMMDD         
        Y.DATA.CONCAT := '*' : Y.PJ.ARR.STATUS       
        Y.DATA.CONCAT := '*' : Y.PJ.PRODUCT.STATUS   
        Y.DATA.CONCAT := '*' : Y.PJ.TOT.PAYMENT      
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.DATES        
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.DEFER.DATES  
        Y.DATA.CONCAT := '*' : Y.PJ.TOT.CAP                   
                Y.DATA.CONCAT := '*' : Y.PJ.TOTAL.PRINCIPAL  
                Y.DATA.CONCAT := '*' : Y.PJ.TOTAL.INTEREST
                Y.DATA.CONCAT := '*' : Y.PJ.TOTAL.CHARGE
        Y.DATA.CONCAT := '*' : Y.PJ.TOTAL.TAX
                Y.DATA.CONCAT := '*' : Y.PJ.TOTAL.PAY                   
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.TYPES        
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.METHODS      
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.TYPE.AMTS    
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.PROPS        
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.PROP.AMTS    
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.OUTS
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.TYPES.DESCR
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.PROPS.DESCR
        Y.DATA.CONCAT := '*' : Y.PJ.DUE.PROPS.CLASS
        Y.DATE.CONCAT := '*' : Y.PJ.TOTAL.OT.TYPE
        Y.DATA.CONCAT := '*' : Y.PJ.TOTAL.OT.PROPS
        Y.DATA.CONCAT := '*' : Y.PJ.OVERRIDE         
        Y.DATA.CONCAT := '*' : Y.PJ.RECORD.STATUS    
        Y.DATA.CONCAT := '*' : Y.PJ.CURR.NO          
        Y.DATA.CONCAT := '*' : Y.PJ.INPUTTER         
        Y.DATA.CONCAT := '*' : Y.PJ.DATE.TIME        
        Y.DATA.CONCAT := '*' : Y.PJ.AUTHORISER       
        Y.DATA.CONCAT := '*' : Y.PJ.CO.CODE          
        Y.DATA.CONCAT := '*' : Y.PJ.DEPT.CODE        
        Y.DATA.CONCAT := '*' : Y.PJ.AUDITOR.CODE     
        Y.DATA.CONCAT := '*' : Y.PJ.AUDIT.DATE.TIME  
        Y.DATA<-1> = Y.DATA.CONCAT
    RETURN

PROCESS.TYPES:
    *DEBUG
           Y.PJ.TOT.CAP                   = ''
    Y.PJ.DUE.TYPES.DESCR   = ''
    Y.PJ.TOTAL.OT.TYPE     = ''     
    Y.COUNT.VM = 0
    Y.COUNT.SM = 0

    Y.COUNT.VM  = DCOUNT(Y.PJ.DUE.TYPES<1>, @VM)

    *INTERNAL 
    Y.DUE.METHODS    = ''
    Y.PT.DESCRIPTION = ''
    
    FOR Y.VM = 1 TO Y.COUNT.VM
        Y.COUNT.SM  = DCOUNT(Y.PJ.DUE.TYPES<1,Y.VM>,@SVM)
        Y.PJ.TOT.CAP<1,Y.VM> = 0
        FOR Y.SM = 1 TO Y.COUNT.SM
        
            ID.PAYMEDNT.TYPE = Y.PJ.DUE.TYPES<1,Y.VM,Y.SM>
        
            CALL F.READ(FN.PAYMENT.TYPE,  ID.PAYMEDNT.TYPE,  R.PAYMENT.TYPE,  F.PAYMENT.TYPE,  PAYMENT.TYPE.ERR)
        
            Y.PT.DESCRIPTION = R.PAYMENT.TYPE<AA.PT.DESCRIPTION,Y.USR.LANGUAGE>
            IF Y.USR.LANGUAGE = 1 OR  Y.PT.DESCRIPTION = '' THEN
                Y.PJ.DUE.TYPES.DESCR<1,Y.VM,Y.SM> = R.PAYMENT.TYPE<AA.PT.DESCRIPTION> 
            END
            ELSE
                Y.PJ.DUE.TYPES.DESCR<1,Y.VM,Y.SM> = Y.PT.DESCRIPTION
            END 
        
            *FOR DEBUG PURPOSE
            *DUE_CAPITALISE_PAY_MAINTAIN_ MUST BE DUE OR CAPITALISE BASE ON PAYMENT.METHOD FIELD HELP OF AA.ARR.PAYMENT.SCHEDULE
            Y.DUE.METHODS = Y.PJ.DUE.PROPS.CLASS<1,Y.VM,Y.SM>
        
            BEGIN CASE
            CASE Y.DUE.METHODS  EQ 'CAPITALISE'
                Y.PJ.TOT.CAP<1,Y.VM> += Y.PJ.DUE.PROP.AMTS<1,Y.VM,Y.SM>
            CASE 1
                *FOR DEBUG PURPOSE
                Y.PJ.TOTAL.OT.TYPE<1,Y.VM> +=  Y.PJ.DUE.TYPE.AMTS<1,Y.VM,Y.SM>
            END CASE   

        NEXT Y.SM
    NEXT Y.VM

    *DEBUG
    RETURN

PROCESS.PROPS:
    *DEBUG
    Y.PJ.DUE.PROPS.DESCR   = ''
    Y.PJ.DUE.PROPS.CLASS   = ''
        Y.PJ.TOTAL.PRINCIPAL   = ''
        Y.PJ.TOTAL.INTEREST           = ''
        Y.PJ.TOTAL.CHARGE           = ''
    Y.PJ.TOTAL.TAX             = ''
        Y.PJ.TOTAL.PAY                   = '' 
    Y.PJ.TOTAL.OT.PROPS    = '' 
    Y.COUNT.VM = 0
    Y.COUNT.SM = 0


    Y.COUNT.VM  = DCOUNT(Y.PJ.DUE.PROPS<1>,@VM)

    *INTERNAL 
    Y.PROPS.CLASS = ''
    Y.PROP.DESCRIPTION = ''

    
    FOR Y.VM = 1 TO Y.COUNT.VM
        Y.COUNT.SM  = DCOUNT(Y.PJ.DUE.PROPS<1,Y.VM>,@SM)
        FOR Y.SM = 1 TO Y.COUNT.SM

            ID.AA.PROPERTY = Y.PJ.DUE.PROPS<1,Y.VM,Y.SM>        
        
            CALL F.READ(FN.AA.PROPERTY,   ID.AA.PROPERTY,   R.AA.PROPERTY,   F.AA.PROPERTY,   AA.PROPERTY.ERR)
        
            Y.PROP.DESCRIPTION  = R.AA.PROPERTY<AA.PROP.DESCRIPTION,Y.USR.LANGUAGE>
            IF Y.USR.LANGUAGE = 1 OR  Y.PROP.DESCRIPTION  = '' THEN
                Y.PJ.DUE.PROPS.DESCR<1,Y.VM,Y.SM> = R.AA.PROPERTY<AA.PROP.DESCRIPTION>
            END    
            ELSE
                Y.PJ.DUE.PROPS.DESCR<1,Y.VM,Y.SM> = Y.PROP.DESCRIPTION 
            END 

            Y.PJ.DUE.PROPS.CLASS<1,Y.VM,Y.SM> = R.AA.PROPERTY<AA.PROP.PROPERTY.CLASS> 
            Y.PROPS.CLASS = Y.PJ.DUE.PROPS.CLASS<1,Y.VM,Y.SM>
        
            BEGIN CASE
            CASE Y.PROPS.CLASS EQ 'ACCOUNT'
                Y.PJ.TOTAL.PRINCIPAL<1,Y.VM> += Y.PJ.DUE.PROP.AMTS<1,Y.VM,Y.SM>
            CASE Y.PROPS.CLASS EQ 'INTEREST'
                Y.PJ.TOTAL.INTEREST<1,Y.VM> += Y.PJ.DUE.PROP.AMTS<1,Y.VM,Y.SM>
            CASE Y.PROPS.CLASS EQ 'CHARGE'
                Y.PJ.TOTAL.CHARGE<1,Y.VM> += Y.PJ.DUE.PROP.AMTS<1,Y.VM,Y.SM>
            CASE Y.PROPS.CLASS EQ 'TAX'
                Y.PJ.TOTAL.TAX<1,Y.VM> += Y.PJ.DUE.PROP.AMTS<1,Y.VM,Y.SM>
            CASE 1
                *FOR DEBUG PURPOSE
                Y.PJ.TOTAL.OT.PROPS <1,Y.VM> += Y.PJ.DUE.PROP.AMTS<1,Y.VM,Y.SM>
            END CASE       
        
        NEXT Y.SM
    NEXT Y.VM
    *DEBUG
    
    RETURN

    RETURN
END
