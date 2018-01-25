/**********************************************************************
 *   PRODUCT:   SAS
 *   VERSION:   9.1
 *   CREATOR:   External File Interface
 *   DATE:      04OCT07
 *   DESC:      Generated SAS Datastep Code
 *   TEMPLATE SOURCE:  (None Specified.)
 *  
 *
 *  
 *
 **********************************************************************  
 *	*주의 사항
 *  	현재 스크립트 파일은 파일명만 출력되어 있습니다.
 *		따라서, 저장된 추출 결과 파일의 경로를 'infile'에 추가하여야 합니다.
 *  	또한, 다년도 추출 결과 파일은 파일명 뒤에 해당년도가 
 *		'_YYYY' 형태로 추가되므로 역시 'infile'에 추가 하여야 합니다.
 *  
 *		SAS 스크립트는 SAS에서 파일 경로와 파일명만 수정하면 바로 실행가능하며,
 *		데이터셋 생성 후에 SAS의 여러 가지 분석 기능을 사용할 수 있습니다.
 *
 ***********************************************************************/
    data WORK.MDIS                                    ;
    %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
    infile 'extr_submania_20180124_79624_2016.txt' delimiter = ',' MISSOVER DSD lrecl=32767 ;
        informat C1 $3. ;
        informat C2 $3. ;
        informat C3 $1. ;

        format C1 $3. ;
        format C2 $3. ;
        format C3 $1. ;
    input
        C1 $
        C2 $
        C3 $
     ;

	label         C1 = '전입행정_시군구'
        C2 = '전출행정_시군구'
        C3 = '세대주_성별';

    run;