CREATE TABLE product_type (
    name VARCHAR(255) PRIMARY KEY NOT NULL
);

CREATE TABLE product (
    name VARCHAR(255) NOT NULL,
    cost INTEGER NOT NULL,
    price INTEGER NOT NULL,
    type VARCHAR(255) NOT NULL,
    brand VARCHAR(255) NOT NULL,
    description TEXT NOT NULL,
    deleted BOOLEAN NOT NULL DEFAULT 'f',
    created_by VARCHAR(255) NOT NULL,
    deleted_by VARCHAR(255),
    created_on TIMESTAMP DEFAULT NOW() NOT NULL,
    deleted_on TIMESTAMP,

    PRIMARY KEY (name, created_on),
    FOREIGN KEY (type) REFERENCES product_type(name),
    CHECK (created_on < deleted_on)
);

CREATE TABLE product_count (
    name VARCHAR(255) PRIMARY KEY NOT NULL,
    count INTEGER NOT NULL
);

CREATE TABLE product_count_log (
    name VARCHAR(255) NOT NULL,
    count INTEGER NOT NULL,
    updated_by VARCHAR(255) NOT NULL,
    updated_on TIMESTAMP DEFAULT NOW() NOT NULL,

    PRIMARY KEY (name, updated_by, updated_on)
);
