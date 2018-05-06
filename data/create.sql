DROP TABLE address;

CREATE TABLE address (
        id INT NOT NULL AUTO_INCREMENT
       ,name nvarchar(20) NOT NULL
       ,namekana nvarchar(40) NOT NULL
       ,zipcode nvarchar(7) NOT NULL
       ,pref nvarchar(4) NOT NULL
       ,addr1 nvarchar(20) NOT NULL
       ,addr2 nvarchar(20) NOT NULL
       ,tel  nvarchar(20) NOT NULL
       ,PRIMARY KEY (id)
);
