CREATE TYPE Fraction AS (
  n INTEGER,
  d INTEGER
);

/*greatest common divider eucledian algorithm*/
CREATE OR REPLACE FUNCTION GCDiv(a INTEGER, b INTEGER) 
RETURNS INTEGER AS 
$$ 
DECLARE 
    c INTEGER; 
BEGIN 
    IF (a IS NULL OR b IS NULL OR (a = 0 AND b = 0)) THEN 
        RETURN 0;
    END IF;
    IF (a = 0 OR b = 0) THEN 
        RETURN ABS(a) + ABS(b); 
    END IF; 
    IF (ABS(a) < ABS(b)) THEN 
        c = ABS(a); 
        a = ABS(b); 
        b = c; 
    ELSE 
        a = ABS(a); 
        b = ABS(b); 
    END IF; 
    c = a % b; 
    WHILE c > 0 LOOP 
        a = b; 
        b = c; 
        c = a % b; 
    END LOOP; 
    RETURN b; 
END; 
$$ LANGUAGE 'plpgsql';

/*Lowest common denominator*/ 
CREATE OR REPLACE FUNCTION LCDenom(a INTEGER, b INTEGER) 
RETURNS INTEGER AS 
$$ 
BEGIN 
    RETURN a / GCDiv(a, b) * b; 
END; 
$$ LANGUAGE 'plpgsql'; 

/*Simplifies fraction to canonical form*/
CREATE OR REPLACE FUNCTION simplify(a Fraction) RETURNS Fraction AS 
$$ 
DECLARE 
    gcd INTEGER; 
BEGIN 
    LOOP 
        gcd = GCDiv(a.n, a.d); 
        EXIT WHEN gcd = 0; 
        EXIT WHEN gcd = 1; 
        a.n = a.n / gcd; 
        a.d = a.d / gcd; 
    END LOOP; 
    IF (a.d < 0) THEN /*always remove the minus FROM the denominator, !!other gt/gte/lt/lte relies on thsi behaviour*/
        a.n = -a.n;
        a.d = -a.d;
    END IF;
    RETURN a;
END; 
$$ LANGUAGE 'plpgsql'; 

/*casts to other types*/
CREATE FUNCTION fraction_to_numeric(Fraction) RETURNS NUMERIC LANGUAGE SQL AS 'SELECT (($1).n::NUMERIC / ($1).d::NUMERIC);'; /*Can introduce rounding errors*/
CREATE CAST (Fraction AS NUMERIC) WITH FUNCTION fraction_to_numeric(Fraction) AS IMPLICIT; 

CREATE FUNCTION fraction_to_float8(Fraction) RETURNS FLOAT8 LANGUAGE SQL AS 'SELECT (($1).n::FLOAT8 / ($1).d::FLOAT8);'; /*Can introduce rounding errors*/
CREATE CAST (Fraction AS FLOAT8) WITH FUNCTION fraction_to_float8(Fraction) AS IMPLICIT; 

CREATE FUNCTION integer_to_fraction(INTEGER) RETURNS Fraction LANGUAGE SQL AS 'SELECT ($1,1)::Fraction;'; 
CREATE CAST (INTEGER AS Fraction) WITH FUNCTION integer_to_fraction(INTEGER) AS IMPLICIT; 

/*sum of two fractions*/ 
CREATE OR REPLACE FUNCTION fraction_add(a Fraction, b Fraction) RETURNS Fraction AS 
$$ 
DECLARE 
    c Fraction; 
BEGIN 
    c.d = LCDenom(a.d, b.d); 
    c.n = a.n * (c.d / a.d); 
    c.n = c.n + b.n * (c.d / b.d); 
    c = simplify(c); 
    RETURN c; 
END; 
$$ LANGUAGE 'plpgsql'; 

/*difference of two fractions*/ 
CREATE OR REPLACE FUNCTION fraction_sub(a Fraction, b Fraction) RETURNS Fraction AS 
$$ 
DECLARE 
    c Fraction; 
BEGIN 
    c.d = LCDenom(a.d, b.d); 
    c.n = a.n * (c.d / a.d); 
    c.n = c.n - b.n * (c.d / b.d);
    c = simplify(c);
    RETURN c;
END; 
$$ LANGUAGE 'plpgsql'; 

/*multiplication of two fractions*/ 
CREATE OR REPLACE FUNCTION fraction_mul(a Fraction, b Fraction) RETURNS Fraction AS 
$$ 
DECLARE 
    c Fraction; 
BEGIN 
    c.n = a.n * b.n;
    c.d = a.d * b.d;
    c = simplify(c);
    RETURN c;
END; 
$$ LANGUAGE 'plpgsql'; 

/*division of two fractions*/ 
CREATE OR REPLACE FUNCTION fraction_div(a Fraction, b Fraction) RETURNS Fraction AS 
$$ 
DECLARE 
    c Fraction; 
BEGIN 
    c.n = a.n * b.d;
    c.d = a.d * b.n;
    c = simplify(c);
    RETURN c;
END; 
$$ LANGUAGE 'plpgsql'; 

/*absolute value of a fraction*/ 
CREATE OR REPLACE FUNCTION fraction_abs(a Fraction) RETURNS Fraction AS 
$$ 
DECLARE 
    c Fraction; 
BEGIN 
    IF ((a.n::NUMERIC / a.d::NUMERIC) < 0) THEN 
        /*prevents returning -a.n/-a.d, which is technically correct but maybe jarring*/ 
        IF (a.d < 0) THEN 
            c.n = a.n; 
            c.d = -a.d; 
        ELSE 
            c.n = -a.n; 
            c.d = a.d; 
        END IF; 
        RETURN c; 
    ELSE 
        /*prevents returning -a.n/-a.d, which is technically correct but maybe jarring*/ 
        IF (a.d < 0) THEN 
            c.n = -a.n; 
            c.d = -a.d; 
            RETURN c;
        ELSE 
            RETURN a; 
        END IF; 
    END IF; 
END; 
$$ LANGUAGE 'plpgsql'; 

/*absolute value of a fraction*/ 
CREATE OR REPLACE FUNCTION fraction_pow(a Fraction, p INTEGER) RETURNS Fraction AS 
$$ 
DECLARE 
    c Fraction; 
BEGIN 
    c.n = power(a.n, p);
    c.d = power(a.d, p);
    c = simplify(c);
    RETURN c;
END; 
$$ LANGUAGE 'plpgsql'; 

/*check equality, may look as a bit of cheat, but the increased accuracy of float8 makes this safe for the ranges the nominator and denominator can be in. It's definately more accurate to simplify a and b and then compare them,
 but simplify is expensive and equality checking is VERY VERY common, so this is much quicker*/ 
/*
 CREATE OR REPLACE FUNCTION fraction_equal(a Fraction, b Fraction) RETURNS BOOLEAN AS 
$$ 
BEGIN 
	RETURN (a::float8 = b::float8);
END; 
$$ LANGUAGE 'plpgsql'; 
*/

CREATE OR REPLACE FUNCTION fraction_equal(a Fraction, b Fraction) RETURNS BOOLEAN AS 
$$ 
BEGIN 
	RETURN (a - b).n = 0; 
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE OR REPLACE FUNCTION gt(a Fraction, b Fraction) RETURNS BOOLEAN AS 
$$ 
BEGIN 
     RETURN (a - b).n > 0;
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE OR REPLACE FUNCTION gte(a Fraction, b Fraction) RETURNS BOOLEAN AS 
$$ 
BEGIN 
     RETURN (a - b).n >= 0;
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE OR REPLACE FUNCTION lt(a Fraction, b Fraction) RETURNS BOOLEAN AS 
$$ 
BEGIN 
     RETURN (a - b).n < 0;
END; 
$$ LANGUAGE 'plpgsql';

CREATE OR REPLACE FUNCTION lte(a Fraction, b Fraction) RETURNS BOOLEAN AS 
$$ 
BEGIN 
     RETURN (a - b).n <= 0;
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE OR REPLACE FUNCTION min(a Fraction, b Fraction) RETURNS Fraction AS 
$$ 
BEGIN 
    IF (a::float8 < b::float8) THEN 
        RETURN a; 
    ELSE 
        RETURN b; 
    END IF; 
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE OR REPLACE FUNCTION max(a Fraction, b Fraction) RETURNS Fraction AS 
$$ 
BEGIN 
    IF (a::float8 < b::float8) THEN 
        RETURN b; 
    ELSE 
        RETURN a; 
    END IF; 
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE OPERATOR + (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = fraction_add,
     commutator = +
);

CREATE OPERATOR - (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = fraction_sub,
     commutator = -
);

CREATE OPERATOR * (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = fraction_mul,
     commutator = *
);

CREATE OPERATOR / (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = fraction_div,
     commutator = /
);

CREATE OPERATOR > (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = gt,
     commutator = >
);

CREATE OPERATOR >= (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = gte,
     commutator = >=
);

CREATE OPERATOR < (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = lt,
     commutator = <
);

CREATE OPERATOR <= (
     leftarg = Fraction,
     rightarg = Fraction,
     procedure = lte,
     commutator = <=
);

CREATE OPERATOR @ (
     rightarg = Fraction,
     procedure = fraction_abs 
); 

CREATE OPERATOR ^ ( 
     leftarg = Fraction, 
     rightarg = INTEGER, 
     procedure = fraction_pow, 
     commutator = ^ 
); 

CREATE OPERATOR = ( 
     leftarg = Fraction, 
     rightarg = Fraction, 
     procedure = fraction_equal, 
     commutator = = 
);

/*aggregate functions*/
CREATE AGGREGATE sum(Fraction) (
     sfunc = fraction_add,
     stype = Fraction,
     initcond = '(0,1)',
     msfunc = fraction_add,
     minvfunc = fraction_sub,
     mstype = Fraction,
     minitcond = '(0,1)'
);

/*Will actually break if all fractions are > (1073741823/1), if you want to  change the value below you'll also need to change the algorithm*/
CREATE AGGREGATE min(Fraction) (
     sfunc = min,
     stype = Fraction,
     initcond = '(2147483647, 1)' /*half of maximum value rounded down*/
);

CREATE AGGREGATE max(Fraction) (
     sfunc = max,
     stype = Fraction,
     initcond = '(-2147483648, 1)'  /*half of minimum value rounded down*/
);

CREATE OR REPLACE FUNCTION calculate_avg(arr Fraction[]) RETURNS Fraction AS 
$$ 
DECLARE 
    sum Fraction; 
    el Fraction;
BEGIN 
    sum = (0,1); 
    FOREACH el IN ARRAY arr LOOP 
        sum = sum + el; 
    END LOOP; 
    sum.d = sum.d * array_length(arr, 1);
    sum = simplify(sum); 
    RETURN sum; 
END; 
$$ LANGUAGE 'plpgsql'; 

CREATE AGGREGATE avg(Fraction) (
    sfunc = array_append,
    stype = Fraction[],
    initcond = '{}',
    finalfunc = calculate_avg
);


