CREATE TABLE product_type (
    name VARCHAR(255) PRIMARY KEY NOT NULL
);

CREATE TABLE product (
    name VARCHAR(255) NOT NULL PRIMARY KEY,
    type VARCHAR(255) NOT NULL,
    brand VARCHAR(255) NOT NULL,
    description TEXT NOT NULL,

    FOREIGN KEY (type) REFERENCES product_type(name)
);

CREATE TABLE price_type (
    name VARCHAR(255) PRIMARY KEY
);

CREATE TABLE product_variation (
    product_name VARCHAR(255) NOT NULL,
    variation VARCHAR(255) NOT NULL,
    cost INTEGER NOT NULL,

    FOREIGN KEY (product_name) REFERENCES product(name),
    PRIMARY KEY (product_name, variation)
);

CREATE TABLE product_price (
    product_name VARCHAR(255) NOT NULL,
    variation VARCHAR(255) NOT NULL,
    price_type VARCHAR(255) NOT NULL,
    price INTEGER NOT NULL,
    count INTEGER NOT NULL,

    FOREIGN KEY (product_name, variation)
        REFERENCES product_variation(product_name, variation),
    FOREIGN KEY (price_type) REFERENCES price_type(name),
    PRIMARY KEY (product_name, variation, price_type)
);
