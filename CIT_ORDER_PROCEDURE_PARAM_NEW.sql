create or replace PROCEDURE CIT_ORDER_PROCEDURE_PARAM_NEW
(
  ORDER_START_DATE IN DATE
) AS

--    V_CN_ID NT_ORDER.CN_ID%TYPE DEFAULT '';
    CN_ID_VAR CIT_OPPORTUNITIES.CN_ID%TYPE DEFAULT '';
--    STATE_PAY DCSPP_PAY_GROUP.STATE%TYPE DEFAULT '';
  V_SEQ_REFIL NUMBER;
  V_BOOLEAN_EXIST NUMBER;
  v_code NUMBER;
  v_errm VARCHAR2(200);

  pragma Autonomous_transaction;

BEGIN

  DELETE FROM CIT_OPPORTUNITIES WHERE ORDER_DATE >= ORDER_START_DATE;
  DELETE FROM CIT_REFIL r where r.order_id = (select distinct o.order_id from DCSPP_ORDER o where o.order_id = r.order_id and o.SUBMITTED_DATE >= ORDER_START_DATE);

  FOR ORDER_VO IN (select distinct o.ORDER_ID,
                                   o.STATE,
                                   o.SUBMITTED_DATE,
                                   o.PROFILE_ID,
                                   nt.CN_ID
                     from DCSPP_ORDER o
                    inner join DCSPP_PAY_GROUP pg
                            on (pg.ORDER_REF = o.ORDER_ID)
                    inner join NT_ORDER nt
                            on (nt.ORDER_ID = o.ORDER_ID)
                    where o.STATE in ('SUBMITTED', 'NO_PENDING_ACTION')
                      and pg.STATE = 'SETTLED'
                      and o.SUBMITTED_DATE >= ORDER_START_DATE
                      and o.SUBMITTED_DATE > ADD_MONTHS(sysdate, -12)) LOOP

--    IF (ORDER_VO.SUBMITTED_DATE >  ADD_MONTHS (sysdate, -12) )THEN
/*        BEGIN
        SELECT CN_ID INTO V_CN_ID FROM NT_ORDER WHERE ORDER_ID = ORDER_VO.ORDER_ID;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          V_CN_ID := NULL;
        END;*/
--        IF V_CN_ID IS NOT NULL  THEN
        V_SEQ_REFIL := NULL;
        V_BOOLEAN_EXIST := 0;
        -- OFFER AGAIN
        BEGIN
         FOR I IN (
          --TABELA TEMPORARIA
          with temp as (
              --order regular.
              SELECT  distinct 'R' AS CODE, o.ORDER_ID, pr.SKU_ORDER,
                (
                  select  IT_REFIL.QUANTITY from DCSPP_ORDER_ITEM OI
                  inner join dcspp_item IT_REFIL on (OI.COMMERCE_ITEMS = IT_REFIL.COMMERCE_ITEM_ID)
                  where o.ORDER_ID = OI.ORDER_ID and IT_REFIL.CATALOG_REF_ID = pr.SKU_REBUY
                ) * ITTPR.EXPIRATION as EXPIRATION , PR.SKU_REBUY
                from DCSPP_ORDER o
                 inner join DCSPP_ORDER_ITEM itOrder on (o.ORDER_ID = itOrder.ORDER_ID)
                 inner join dcspp_item it on (itOrder.COMMERCE_ITEMS = it.COMMERCE_ITEM_ID)
                 inner join CIT_PRODUCT_REFIL pr on (pr.SKU_ORDER =  it.CATALOG_REF_ID)
                 --REFIL IRFORMATION
                 INNER JOIN CIT_PRODUCT_REFIL ITTPR ON (ITTPR.SKU_ORDER = pr.SKU_REBUY)
                WHERE pr.SKU_ORDER <> pr.SKU_REBUY AND O.ORDER_ID = ORDER_VO.ORDER_ID
              union
              --order refil with regular.
              SELECT DISTINCT 'F'  AS CODE, o.ORDER_ID, pr.SKU_ORDER, 0 AS EXPIRATION, PR.SKU_REBUY
              from DCSPP_ORDER o
               inner join DCSPP_ORDER_ITEM itOrder on (o.ORDER_ID = itOrder.ORDER_ID)
               inner join dcspp_item it on (itOrder.COMMERCE_ITEMS = it.COMMERCE_ITEM_ID)
               inner join CIT_PRODUCT_REFIL pr on (pr.SKU_ORDER =  it.CATALOG_REF_ID)
               --PARENT
               INNER JOIN CIT_PRODUCT_REFIL PARENT_REFIL ON (pr.SKU_REBUY =  it.CATALOG_REF_ID)
               inner join DCSPP_ORDER_ITEM OI ON (o.ORDER_ID = OI.ORDER_ID)
               inner join dcspp_item IT_REFIL on (OI.COMMERCE_ITEMS = IT_REFIL.COMMERCE_ITEM_ID AND IT_REFIL.CATALOG_REF_ID = PARENT_REFIL.SKU_ORDER)
              WHERE PR.SKU_ORDER = PR.SKU_REBUY AND O.ORDER_ID = ORDER_VO.ORDER_ID
            )
            --INICIO REAL SELECT
            select o.ORDER_ID, pr.SKU_REBUY as SKU, SKU.DISPLAY_NAME as DESCRIPTION , pr.REFILLABLE as REFILLABLE,
            o.profile_id as CF_ID,  NT.CN_ID as CN_ID,
            -- BUSCA O VALOR DA DATA CONSIDERANDO O TMEPO DE EXPIRAC?O SE TCODE FOR R SOMAR O TEMPO DOS REFIS
            to_date(CASE T.CODE WHEN 'R' THEN TO_CHAR(o.SUBMITTED_DATE + pr.expiration + case when T.EXPIRATION is null then 0 else  T.EXPIRATION end , 'dd/MM/yyyy' ) ELSE TO_CHAR(o.SUBMITTED_DATE + pr.expiration, 'dd/MM/yyyy' ) END , 'dd/MM/yyyy') as EXPIRATION
            from DCSPP_ORDER O
              INNER JOIN NT_ORDER NT ON O.ORDER_ID = NT.ORDER_ID
              inner join DCSPP_ORDER_ITEM itOrder on (o.ORDER_ID = itOrder.ORDER_ID)
              inner join dcspp_item it on (itOrder.COMMERCE_ITEMS = it.COMMERCE_ITEM_ID)
              inner join CIT_PRODUCT_REFIL PR on (pr.SKU_ORDER =  it.CATALOG_REF_ID)
              INNER JOIN DCSPP_PAY_GROUP PG ON (PG.ORDER_REF = o.ORDER_ID)
              INNER JOIN temp T ON (T.ORDER_ID = O.ORDER_ID AND T.SKU_ORDER = pr.SKU_ORDER)			  
			--Mudança
			  INNER JOIN NT_SKU NTP ON  (NTP.SKU_CODE = T.SKU_REBUY)
			  INNER JOIN DCS_SKU SKU ON (SKU.SKU_ID = NTP.SKU_ID)    
             ---			  
            where  pr.SKU_REBUY is not null
           AND itOrder.ORDER_ID = ORDER_VO.ORDER_ID
           AND  o.SUBMITTED_DATE is not null
           and (
              -- FILTRO DA DATA, PARA N?O INSERIR REGISTROS ANTIGOS
              (T.CODE = 'F' AND o.SUBMITTED_DATE + pr.expiration + 60 >= sysdate
                and T.SKU_REBUY not in (
                  select t2.SKU_REBUY from temp t2 where T2.ORDER_ID = O.ORDER_ID
                    and t2.code = 'R' and t2.SKU_REBUY = T.SKU_REBUY
                  ))
              OR
              (T.CODE = 'R' AND (o.SUBMITTED_DATE + pr.expiration + case when T.EXPIRATION is null then 0 else  T.EXPIRATION end + 60 >= sysdate))
            )
            UNION
            -- PEGAR OS PRODUTOS DEFAULSTS QUE N?O TEM NA TABBELA
            select o.ORDER_ID, I.CATALOG_REF_ID as SKU, DS.DISPLAY_NAME as DESCRIPTION , 'N' as REFILLABLE,
            o.profile_id as CF_ID,  NT.CN_ID as CN_ID, o.SUBMITTED_DATE + 90 as EXPIRATION
            from DCSPP_ORDER O
              INNER JOIN NT_ORDER NT ON O.ORDER_ID = NT.ORDER_ID
              inner join DCSPP_ORDER_ITEM itOrder on (o.ORDER_ID = itOrder.ORDER_ID)
              inner join dcspp_item I on (itOrder.COMMERCE_ITEMS = I.COMMERCE_ITEM_ID)
              LEFT JOIN CIT_PRODUCT_REFIL pr on (pr.SKU_ORDER =  I.CATALOG_REF_ID)		  
			--Mudança
   			  INNER JOIN NT_PRODUCT NTP ON (NTP.PRODUCT_ID = I.PRODUCT_ID)
			  INNER JOIN DCS_PRODUCT DS ON (DS.PRODUCT_ID = NTP.PRODUCT_ID)
			  ---
              INNER JOIN DCSPP_PAY_GROUP PG ON (PG.ORDER_REF = o.ORDER_ID)
            WHERE PR.SKU_ORDER IS NULL and o.SUBMITTED_DATE + 150 >= sysdate
            AND O.ORDER_ID = ORDER_VO.ORDER_ID and o.SUBMITTED_DATE is not null
         ) LOOP
          -- VERIFICA SE E O MESMO REGISTRO AINDA PARA N?O PEGAR OUTRA SEQUENCE, APENAS UMA VEZ
          IF(V_SEQ_REFIL IS NULL) THEN
            -- VERIFICA SE O REGISTRO JA EXISTE
            BEGIN
              SELECT ID_KEY
              INTO   V_BOOLEAN_EXIST
              FROM   CIT_REFIL
              WHERE CN_ID = I.CN_ID AND CF_ID = I.CF_ID AND SKU=I.SKU AND ORDER_ID = I.ORDER_ID;
            EXCEPTION
               WHEN OTHERS THEN
                 V_BOOLEAN_EXIST := 0;
                 SELECT REFIL_SEQ.NEXTVAL
                   INTO V_SEQ_REFIL
                   FROM dual;
            END;
            --SE N?O EXISITR BUSCA NA SEQUENCE
/*            IF (V_BOOLEAN_EXIST = 0) THEN
              SELECT REFIL_SEQ.NEXTVAL
              INTO   V_SEQ_REFIL
              FROM   dual;
            END IF;*/
          END IF;

          BEGIN
            --NOVO REFIL, N?O EXISTIA ANTES
            IF(V_BOOLEAN_EXIST = 0) THEN
              INSERT INTO CIT_REFIL (ID_KEY,DESCRIPTION,SKU,REFILLABLE,EXPIRATION,CF_ID,CN_ID, ORDER_ID)
                VALUES ( V_SEQ_REFIL, I.DESCRIPTION,I.SKU,I.REFILLABLE,I.EXPIRATION, I.CF_ID, I.CN_ID, I.ORDER_ID);
            ELSE
              -- REFIL JA EXISTE, SOMENTE ATUALIZA A DATA.
              V_SEQ_REFIL := V_BOOLEAN_EXIST;
              UPDATE CIT_REFIL SET REFILLABLE = I.REFILLABLE, EXPIRATION = I.EXPIRATION WHERE CN_ID = I.CN_ID AND CF_ID = I.CF_ID AND SKU=I.SKU AND ORDER_ID = I.ORDER_ID;
            END IF;
--            commit;
          EXCEPTION
           WHEN OTHERS THEN
                v_code := SQLCODE;
                v_errm := SUBSTR(SQLERRM, 1, 200);
                INSERT INTO CIT_OPP_ERRORS (ID, ORDER_ID, ERROR, ERROR_DATE, ERROR_CODE, SOURCE)
                       VALUES ( CIT_SEQ_OPP_ERROR.NEXTVAL, I.ORDER_ID, v_errm, SYSDATE, v_code, 'R');
--                commit;
          END;
         END LOOP;

        EXCEPTION
           WHEN OTHERS THEN
              V_SEQ_REFIL:= NULL;
        END;
        COMMIT;         
        -- REMOVE OLD REGISTERS OF CIT_REFIL
--        BEGIN
--          DELETE FROM CIT_REFIL WHERE EXPIRATION + 60 < SYSDATE;
--          COMMIT;
--        EXCEPTION
--          WHEN OTHERS THEN
--            NULL;
--        END;
--        COMMIT;
        -- OFFER ALSO START
        BEGIN
          FOR I IN (
            WITH TEMP_CF_BRAND_ORDER
            AS
             (
              select distinct O.PROFILE_ID as CF_ID, brand.BRAND_ID
              FROM DCSPP_ORDER O
                INNER JOIN DCSPP_ORDER_ITEM OI ON O.ORDER_ID = OI.ORDER_ID
                INNER JOIN DCSPP_ITEM I ON OI.COMMERCE_ITEMS = I.COMMERCE_ITEM_ID
                inner JOIN DCSPP_PAY_GROUP PG on (O.order_id = PG.ORDER_REF)
                inner join oe_product oep on  (oep.PRODUCT_ID = I.PRODUCT_ID)
                inner join oe_brand brand on (brand.BRAND_ID = oep.BRAND_ID)
                inner join nt_order nt on (nt.order_id = o.order_id)
              WHERE PG.STATE = 'SETTLED' and O.STATE in ('SUBMITTED', 'NO_PENDING_ACTION')
                and O.SUBMITTED_DATE >  ADD_MONTHS (sysdate, -12) 
                and O.PROFILE_ID = ORDER_VO.PROFILE_ID 
                and nt.cn_id =  ORDER_VO.Cn_Id
              )
              select  distinct cnd.CN_ID as CN_ID , prof.CF_ID as CF_ID, oep.name AS OFFER, cnd.SUBMITTED_DATE AS ORDER_DATE
              from   cit_offer brand_result
                inner join TEMP_CF_BRAND_ORDER prof on (prof.BRAND_ID = brand_result.brand_order )
                inner join oe_brand oep on (brand_result.BRAND_offer = oep.BRAND_ID)
                inner join (
                  select distinct o.profile_id as CF_ID, nt_sub.CN_ID ,  O.SUBMITTED_DATE
                  from nt_order nt_sub
                    inner join  DCSPP_ORDER O on (nt_sub.order_id = o.order_id)
                  where o.submitted_date = (
                    select max(o_sub.submitted_date) from DCSPP_ORDER o_sub
                    where o_sub.profile_id = o.profile_id
                      and nt_sub.cn_id = ORDER_VO.Cn_Id
                  )
                ) cnd on (cnd.CF_ID = prof.CF_ID)
              where brand_result.interest = (
                select max(brand.interest)
                From TEMP_CF_BRAND_ORDER prof2
                  inner join cit_offer brand on (prof2.BRAND_ID = brand.brand_order )
                where brand.brand_offer not in( select op.BRAND_ID from TEMP_CF_BRAND_ORDER op where op.CF_ID = prof2.CF_ID ) and
                  prof2.CF_ID = prof.CF_ID
              ) AND  brand_result.SUPPORT = (
                 select max(brand.SUPPORT)
                  From TEMP_CF_BRAND_ORDER prof2
                  inner join cit_offer brand on (prof2.BRAND_ID = brand.brand_order )
                  where brand.brand_offer not in( select op.BRAND_ID from TEMP_CF_BRAND_ORDER op where op.CF_ID = prof2.CF_ID ) and
                  prof2.CF_ID = prof.CF_ID AND brand.interest = brand_result.interest
              )  and brand_result.brand_offer not in( select op.BRAND_ID from TEMP_CF_BRAND_ORDER op where op.CF_ID =  prof.CF_ID )
            ) LOOP
              -- VERIFICA SE A CN E O ULTIMO DO CF
              BEGIN
                SELECT MAX(ID_KEY) INTO V_SEQ_REFIL
                FROM CIT_OFFER_AGAIN_VIEW WHERE CF_ID = I.CF_ID AND CN_ID =  I.CN_ID;
              EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  V_SEQ_REFIL := NULL;
              END;

              -- APAGA OS REGISTROS DOS OUTROS CN PARA ESSE CF
              --IF (V_SEQ_REFIL IS NOT NULL) THEN
              --  UPDATE CIT_OPPORTUNITIES SET REFIL = NULL WHERE CF_ID= I.CF_ID;
              --END IF;
              
              -- VERIFICA SE JA EXISTE A OPORTUNIDADE
              BEGIN
                SELECT CN_ID INTO CN_ID_VAR FROM CIT_OPPORTUNITIES WHERE CF_ID = I.CF_ID and CN_ID = I.CN_ID;
              EXCEPTION
                WHEN NO_DATA_FOUND THEN
                  CN_ID_VAR := NULL;
              END;

              BEGIN
                  -- SE EXISTIR APAGA ELA
                  IF CN_ID_VAR IS NOT NULL  THEN
                    DELETE FROM CIT_OPPORTUNITIES WHERE CF_ID = I.CF_ID and CN_ID = I.CN_ID;
                  END IF;
                  -- INSERE A OPORTUNIDADE NOVA
                  INSERT INTO CIT_OPPORTUNITIES (CN_ID, CF_ID, OFFER, REFIL, ORDER_DATE) VALUES (I.CN_ID, I.CF_ID, I.OFFER, V_SEQ_REFIL, I.ORDER_DATE);
--                  commit;
               EXCEPTION
                 WHEN OTHERS THEN
                  v_code := SQLCODE;
                  v_errm := SUBSTR(SQLERRM, 1, 200);
                  INSERT INTO CIT_OPP_ERRORS (ID, ORDER_ID, ERROR, ERROR_DATE, ERROR_CODE, SOURCE)
                         VALUES ( CIT_SEQ_OPP_ERROR.NEXTVAL, ORDER_VO.ORDER_ID, v_errm, SYSDATE, v_code, 'A');
--                commit;
              END;

            END LOOP;
         EXCEPTION
            WHEN OTHERS THEN
               NULL;
         END;
         COMMIT;
--        END IF;
--    END IF;
  END LOOP;
EXCEPTION
  WHEN OTHERS THEN
        NULL;
END CIT_ORDER_PROCEDURE_PARAM_NEW;