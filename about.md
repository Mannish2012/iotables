
Suppose a nation's economy is composed of $n$ sectors. Per sector, consider a **production vector**,
$x$, in $R^n$ that lists it's output per year. The entries in this vector denote the contribution
of every sector (including itself) to it. 

For example, suppose we have a simplified economy that consists of only 3 sectors, *Manufacturing*,
*Agriculture* and *Services*. Consider the table below: 

|Purchased from|Manufacturing|Agriculture|Services|
|------------- |------------- | ----------- | --------|
|Manufacturing | 0.5         | 0.4 | 0.2|
|Agriculture   | 0.4         | 0.2 | 0.1|
|Services      | 0.3         | 0.3 | 0.3|

The columns represent the *contributions* of other sectors to each sector (including itself). 
For example, in our example, $50\\%$ of inputs to the manufacturing sector came from manufacturing 
itself, $40\\%$ from agriculture, and $30\\%$ from services. 

The column sums must be less than 1. 

The basic equation of is
$amount{ produced} = intermediate{ demand} + final{ demand}$

The *amount produced* and the *final demand* are both vectors in $R^n$. The intermediate demand
however, is a matrix. In fact, this matrix, called the **consumption matrix** is the product C**x**,
where $C$ is the matrix of the coefficients of the vectors, and **x**, is the production vector.

The equation is, hence, $x = Cx + d$, where **d** is the *final demand vector*. Now if the matrix
is square (as is the case here), and the columns are independent, then the matrix is nonsingular,
and can be represented as: 

$(I-C)x = d$, where $I$ is the *identity matrix* of dimension *n*. Therefore, as $I-C$ is itself
nonsingular, the final equation is $x = (I-C)^{-1}d$. 

## Why is $(I-c)^{-1}$ important?

The $j_{th}$ column of $(I-c)^{-1}$ shows the amount that every sector has to produce to satisfy
an addditional increase of one unit in the **final demand** from sector *j*. 


