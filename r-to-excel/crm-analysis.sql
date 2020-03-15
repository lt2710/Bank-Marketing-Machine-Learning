/*1. How many customers are listed in the USA?*/ 
SELECT Count(customername) AS TotalCustomers 
FROM   customers 
WHERE  country IN ("USA")


/*2. How many cities are there in the Customers table, broken out by USA vs. non-USA?*/
SELECT city,
       /*count case of US cities*/
       Count( 
       CASE 
              WHEN country IN ("USA") THEN 1 
              ELSE NULL 
       END) AS "USA Cities",  
       /*count case of non-US cities*/
       Count( 
       CASE 
              WHEN country NOT IN ("USA") THEN 1 
              ELSE NULL 
       END) AS "Non-USA Cities"
       /*group the customer table by city*/ 
FROM   ( 
                SELECT   * 
                FROM     customers 
                GROUP BY city) 


/*3. What are the top 5 cities in the USA by total orders?*/
SELECT   c.city, 
         /*count the number of orders*/ 
         Count (*) AS numberoforders 
         /*merge customer and order data*/ 
FROM     ( 
                SELECT * 
                FROM   customers 
                WHERE  country = "USA") AS c 
JOIN 
         ( 
                SELECT customerid, 
                       orderid 
                FROM   orders) AS o 
using    (customerid)
/*group by city after join*/ 
GROUP BY city 
/*order*/ 
ORDER BY numberoforders DESC 
/*get top 5*/
limit 5 

/*4. What are the top 5 US cities by units per transaction (UPT)?*/
SELECT   city, 
         ordertotalquantity/ordertotal AS upt 
/*merge customer, order and oder details data*/
FROM     ( 
                  SELECT   c.city, 
                           Count(*)         AS ordertotal, 
                           SUM(od.quantity) AS ordertotalquantity 
                  FROM     ( 
                                  SELECT * 
                                  FROM   customers 
                                  WHERE  country = "USA") AS c 
                  JOIN 
                           ( 
                                  SELECT customerid, 
                                         orderid 
                                  FROM   orders) AS o 
                  using    (customerid) 
                  JOIN 
                           ( 
                                  SELECT orderdetailid, 
                                         orderid, 
                                         quantity 
                                  FROM   orderdetails) AS od 
                  using    (orderid) 
                  /*group by city after join*/
                  GROUP BY city )
/*order*/  
ORDER BY upt DESC 
/*get top 5*/
limit 5