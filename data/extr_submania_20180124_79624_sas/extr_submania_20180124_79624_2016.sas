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
 *	*���� ����
 *  	���� ��ũ��Ʈ ������ ���ϸ� ��µǾ� �ֽ��ϴ�.
 *		����, ����� ���� ��� ������ ��θ� 'infile'�� �߰��Ͽ��� �մϴ�.
 *  	����, �ٳ⵵ ���� ��� ������ ���ϸ� �ڿ� �ش�⵵�� 
 *		'_YYYY' ���·� �߰��ǹǷ� ���� 'infile'�� �߰� �Ͽ��� �մϴ�.
 *  
 *		SAS ��ũ��Ʈ�� SAS���� ���� ��ο� ���ϸ� �����ϸ� �ٷ� ���డ���ϸ�,
 *		�����ͼ� ���� �Ŀ� SAS�� ���� ���� �м� ����� ����� �� �ֽ��ϴ�.
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

	label         C1 = '��������_�ñ���'
        C2 = '��������_�ñ���'
        C3 = '������_����';

    run;