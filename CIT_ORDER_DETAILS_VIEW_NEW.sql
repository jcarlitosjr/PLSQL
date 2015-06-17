
  CREATE OR REPLACE FORCE VIEW "PAINELCH"."CIT_ORDER_DETAILS_VIEW_NEW" ("CF_ID", "CN_ID", "ORDER_NUMBER", "ORDER_NUMBER_BROKER", "ORDER_STATE", "STATE", "ORDER_DATE", "SKU_ID", "PRODUCT_ID", "DISPLAY_NAME", "QUANTITY", "UN_DISCOUNT", "UN_PRICE", "RAW_SUBTOTAL", "RAW_TOTAL_PRICE", "PROMOTION_ID", "COUPON_ID", "SHIPPING", "ORDER_TOTAL", "ITEM_DISCOUNT", "CUSTOMER_NAME", "CUSTOMER_EMAIL", "CUSTOMER_BIRTHDAY", "CREATION_DATE", "LAST_MODIFIED_DATE", "DELIVER_DATE", "PAYMENT_METHOD", "CITY_STATE", "IMAGE_URL", "INSTALLMENTS", "ORDER_DISCOUNT") AS 
  SELECT DISTINCT
	O.PROFILE_ID CF_ID,
	NT.CN_ID,
	OE.ORDER_ID AS ORDER_NUMBER,
	OE.ORDER_NUMBER AS ORDER_NUMBER_BROKER,
	O.STATE AS ORDER_STATE,
	PG.STATE,
	O.SUBMITTED_DATE ORDER_DATE,
	CATALOG_REF_ID AS SKU_ID,
	I.PRODUCT_ID,
	PR.DISPLAY_NAME,
	I.QUANTITY,
	(CASE WHEN ADJUSTMENT = I.QUANTITY * LIST_PRICE THEN 0 ELSE ADJUSTMENT END) AS UN_DISCOUNT,
	LIST_PRICE AS UN_PRICE,
  OP.RAW_SUBTOTAL,
  P.RAW_TOTAL_PRICE,
	PRICING_MODEL AS PROMOTION_ID,
  NT.COUPON_ID,
	OP.SHIPPING,
    OP.RAW_SUBTOTAL AS ORDER_TOTAL,
    P.ORDER_DISCOUNT as ITEM_DISCOUNT,
	U.FIRST_NAME || ' ' ||
	U.LAST_NAME AS CUSTOMER_NAME,
	U.EMAIL CUSTOMER_EMAIL,
	U.DATE_OF_BIRTH CUSTOMER_BIRTHDAY,
	O.CREATION_DATE,
	O.LAST_MODIFIED_DATE,
	NSG.SCHED_DELIV_DATE DELIVER_DATE,
	PG.PAYMENT_METHOD,
	'MOCK - MK' CITY_STATE,
	'//rede.natura.net/image/sku/produto_no_sacola_67x67/'||CATALOG_REF_ID||'_1.jpg' IMAGE_URL,
  (SELECT OPG.NO_OF_PAYMENTS
   FROM OE_PAY_GROUP OPG
   WHERE OPG.PAYMENT_GROUP_ID = PG.PAYMENT_GROUP_ID
   AND ROWNUM <= 1
  ) INSTALLMENTS,
  (
      select sum(PA.ADJUSTMENT)
      from DCSPP_ORDER_PRICE P
      LEFT JOIN DCSPP_AMTINFO_ADJ ADJ ON ADJ.AMOUNT_INFO_ID = P.AMOUNT_INFO_ID
      LEFT JOIN DCSPP_PRICE_ADJUST PA ON (ADJ.ADJUSTMENTS = PA.ADJUSTMENT_ID and P.RAW_SUBTOTAL <> PA.ADJUSTMENT)
      where O.PRICE_INFO = P.AMOUNT_INFO_ID and PA.ADJUSTMENT is not null AND PA.ADJUSTMENT < 0
  ) as ORDER_DISCOUNT
	FROM DCSPP_ORDER O
	INNER JOIN OE_ORDER OE ON O.ORDER_ID = OE.ORDER_ID
	INNER JOIN NT_ORDER NT ON O.ORDER_ID = NT.ORDER_ID
	INNER JOIN DCSPP_ORDER_ITEM OI ON O.ORDER_ID = OI.ORDER_ID
	LEFT JOIN DCSPP_ORDER_PRICE OP ON O.PRICE_INFO = OP.AMOUNT_INFO_ID
	INNER JOIN DCSPP_ITEM I ON OI.COMMERCE_ITEMS = I.COMMERCE_ITEM_ID
	LEFT JOIN DCSPP_ITEM_PRICE P ON I.PRICE_INFO = P.AMOUNT_INFO_ID
	LEFT JOIN DCSPP_AMTINFO_ADJ ADJ ON ADJ.AMOUNT_INFO_ID = P.AMOUNT_INFO_ID
	
	-------Mudança
	INNER JOIN NT_PRODUCT NTP ON (NTP.PRODUCT_CODE = I.PRODUCT_ID)
	INNER JOIN DCS_PRODUCT PR ON (PR.PRODUCT_ID = NTP.PRODUCT_ID)	
	INNER JOIN DCSPP_PAY_GROUP PG ON O.ORDER_ID = PG.ORDER_REF
	INNER JOIN DPS_USER U ON O.PROFILE_ID = U.ID
	INNER JOIN DCSPP_SHIP_GROUP SG ON O.ORDER_ID = SG.ORDER_REF
    LEFT JOIN NT_SHIP_GROUP NSG ON SG.SHIPPING_GROUP_ID = NSG.SHIPPING_GROUP_ID
	LEFT OUTER JOIN DCSPP_PRICE_ADJUST PA ON ADJ.ADJUSTMENTS = PA.ADJUSTMENT_ID;