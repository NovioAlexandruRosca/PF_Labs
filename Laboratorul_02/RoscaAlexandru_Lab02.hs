-- Nume:  Rosca Alexandru-David
-- Grupa: A4
-- Anul:  II
-- Exercises Done: 1, 2, 3, 4, 5, 7, 8, 9, Bonus1, Bonus2

-- //////////////////////////////////////////////////////////////

-- Exercitiul 1

-- a) si logic

and :: Bool -> Bool -> Bool
and False _ = False
and _ False = False
and True True = True

-- b)  sau logic

or :: Bool -> Bool -> Bool
or True _ = True
or _ True = True
or False False = False

-- c) negatie

not :: Bool -> Bool
not True = False
not False = True

-- d) nand 

nand :: Bool -> Bool -> Bool
nand x y = Main.not (Main.and x y) 

-- ghci> nand True False
-- True
-- ghci> nand True True
-- False
-- ghci> nand False False
-- True

-- e) nor

nor :: Bool -> Bool -> Bool
nor x y = Main.not (Main.or x y)

-- ghci> nor False False
-- True
-- ghci> nor False True
-- False
-- ghci> nor True True
-- False

-- f) implicatie

impl :: Bool -> Bool -> Bool
impl True False = False
impl _ _ = True

-- ghci> impl True False
-- False
-- ghci> impl True True
-- True
-- ghci> impl False False
-- True
-- ghci> impl False True
-- True

-- g) dubla implicatie

impl2 :: Bool -> Bool -> Bool
impl2 True True = True
impl2 False False = True
impl2 _ _ = False

-- ghci> impl2 True True
-- True
-- ghci> impl2 False False
-- True
-- ghci> impl2 False True
-- False
-- ghci> impl2 True False
-- False

-- //////////////////////////////////////////////////////////////

-- Exercitiul 2

hasDivisors :: Integer -> Integer -> Integer -> Bool
hasDivisors n a b | a > b = False
hasDivisors n a b | n `mod` a == 0 = True
hasDivisors n a b = hasDivisors n (a + 1) b

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
isPrime n          = Main.not (hasDivisors n 2 (n `div` 2))

-- ghci> isPrime 1
-- False
-- ghci> isPrime 0
-- False
-- ghci> isPrime 23
-- True
-- ghci> isPrime 24
-- False
-- ghci> isPrime 21
-- False

-- //////////////////////////////////////////////////////////////

-- Exercitiul 3

-- a) Scaderi repetate

cmmdcS :: Integer -> Integer -> Integer
cmmdcS a b | a == b = a
cmmdcS a b | a > b  = cmmdcS (a - b) b
cmmdcS a b          = cmmdcS a (b - a)

-- ghci> cmmdc 9 2
-- 1
-- ghci> cmmdc 9 3
-- 3
-- ghci> cmmdc 9 9
-- 9

-- b) Impartiri repetate

cmmdcI :: Integer -> Integer -> Integer
cmmdcI a b | b == 0 = a
cmmdcI a b          = cmmdcI b (a `mod` b)

-- ghci> cmmdcI 10 3
-- 1
-- ghci> cmmdcI 10 4
-- 2
-- ghci> cmmdcI 10 5
-- 5
-- ghci> cmmdcI 10 10
-- 10

-- c) Stein's algorithm (binary)

cmmdcB :: Integer -> Integer -> Integer
cmmdcB a b | b == 0           = a
cmmdcB a b | a == 0           = b
cmmdcB a b | even a && even b = 2 * cmmdcB (a `div` 2) ( b `div` 2)
cmmdcB a b | even a && odd b  = cmmdcB (a `div` 2) b
cmmdcB a b | odd a && even b  = cmmdcB a (b `div` 2)
cmmdcB a b | a <= b           = cmmdcB a (b - a)
cmmdcB a b                    = cmmdcB (a - b) b

-- ghci> cmmdcB 2 10
-- 2
-- ghci> cmmdcB 3 10
-- 1
-- ghci> cmmdcB 10 3
-- 1
-- ghci> cmmdcB 10 4
-- 2
-- ghci> cmmdcB 10 5
-- 5
-- ghci> cmmdcB 10 10
-- 10

-- //////////////////////////////////////////////////////////////

-- Exercitiul 4

-- DA, folosirea acumulatorilor reprezinta o transformare cvasi-mecanica 
-- care obliga compilatorul sa renunte la folosirea unei stive
-- deoarece toate apelurile recursive sunt in pozitie de coada.

cmmdcA :: Integer -> Integer -> Integer -> Integer
cmmdcA a b n | b == 0 = n
cmmdcA a b n          = cmmdcA b (a `mod` b) b

-- //////////////////////////////////////////////////////////////

-- Exercitiul 5

-- a) Varianta Basic

fibo :: Integer -> Integer
fibo n | n == 0 || n == 1 = 1
fibo n                    = fibo(n - 2) + fibo (n - 1)

-- b) Varianta cu acumulatori

fiboaux :: Integer -> Integer -> Integer -> Integer
fiboaux 0 a _ = a
fiboaux n a b = fiboaux (n - 1) (a + b) a

fibo' :: Integer -> Integer
fibo' n = fiboaux n 0 1


-- fibo' si fibo sunt echivalente deoarce fibo genereaza
-- elementele ca fiind suma dintr ultimi 2 termini consecutivi ai sirului:
-- fibo 3 = 
-- fibo(1) + fibo(2) = 
-- 1 + fibo(2) = 
-- 1 + fibo(0) + fibo(1) = 
-- 1 + 1 + fibo(1) =
-- 1 + 1 + 1 =
-- 3

-- Pe cand fibo' genereaza cate un element pe rand si "calculeaza"
-- valoarea termenului urmator folosind cele 2 variabile(2 termini consecutivi ai sirului lui fibo)
-- folosite ca si acumulatori(E ca si cum construim in mod invers valorile pana ajungem la a n-a valoare)

-- fibo' 3 = 
-- fiboaux 3 0 1 = 
-- fiboaux 2 1 1 = 
-- fiboaux 1 2 1 = 
-- fiboaux 0 3 2 = 
-- 3

-- ||||||||||||||||||||||||||||||||||   EXEMPLU:

-- ghci> fibo' 10
-- 55
-- ghci> fibo' 1000
-- 43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
-- ghci> fibo' 100
-- 354224848179261915075

-- O(logn) Fibo (This is based on the identities of the Lucas sequence)

fiboeficient n = fibo'' 1 0 1 0 n

fibo'' a b q p n | n <= 0 = b
fibo'' a b q p n | even n = fibo'' a b (2 * p * q + q * q) (p * p + q * q) (n `div` 2)
fibo'' a b q p n          = fibo'' (b * q + a * q + a * p) (b * p + a * q) q p (n - 1)

-- ghci> fiboeficient 10000
-- 33644764876431783266621612005107543310302148460680063906564769974680081442166662368155595513633734025582065332680836159373734790483865268263040892463056431887354544369559827491606602099884183933864652731300088830269235673613135117579297437854413752130520504347701602264758318906527890855154366159582987279682987510631200575428783453215515103870818298969791613127856265033195487140214287532698187962046936097879900350962302291026368131493195275630227837628441540360584402572114334961180023091208287046088923962328835461505776583271252546093591128203925285393434620904245248929403901706233888991085841065183173360437470737908552631764325733993712871937587746897479926305837065742830161637408969178426378624212835258112820516370298089332099905707920064367426202389783111470054074998459250360633560933883831923386783056136435351892133279732908133732642652633989763922723407882928177953580570993691049175470808931841056146322338217465637321248226383092103297701648054726243842374862411453093812206564914032751086643394517512161526545361333111314042436854805106765843493523836959653428071768775328348234345557366719731392746273629108210679280784718035329131176778924659089938635459327894523777674406192240337638674004021330343297496902028328145933418826817683893072003634795623117103101291953169794607632737589253530772552375943788434504067715555779056450443016640119462580972216729758615026968443146952034614932291105970676243268515992834709891284706740862008587135016260312071903172086094081298321581077282076353186624611278245537208532365305775956430072517744315051539600905168603220349163222640885248852433158051534849622434848299380905070483482449327453732624567755879089187190803662058009594743150052402532709746995318770724376825907419939632265984147498193609285223945039707165443156421328157688908058783183404917434556270520223564846495196112460268313970975069382648706613264507665074611512677522748621598642530711298441182622661057163515069260029861704945425047491378115154139941550671256271197133252763631939606902895650288268608362241082050562430701794976171121233066073310059947366875

-- //////////////////////////////////////////////////////////////

-- Exercitiul 8

euclidExtins :: Integer -> Integer -> (Integer, Integer, Integer)
euclidExtins a b | b == 0 = (a, 1, 0)
euclidExtins a b          = (d, y, x - (a `div` b) * y) 
                            where (d, x, y) = euclidExtins b (a `mod` b)

-- ghci> euclidExtins 5 10
-- (5,1,0)
-- ghci> euclidExtins 180 150
-- (30,1,-1)

-- //////////////////////////////////////////////////////////////

-- Exercitiul 7

succ :: Integer -> Integer
succ n = n + 1

-- ghci> Main.succ 3
-- 4
-- ghci> Main.succ (-2)
-- -1

-- //////////////////////////////////////////////////////////////

-- Exercitiul 8

-- a) adunarea a doua numere naturale

succAddition :: Integer -> Integer -> Integer
succAddition a b | b == 0 = a
succAddition a b          = succAddition (Main.succ a) (b - 1)

-- ghci> succAddition 2 3
-- 5

-- b) inmultirea a doua numere naturale

succMultiplication :: Integer -> Integer -> Integer
succMultiplication a b | b == 1 = a
succMultiplication a b          = succAddition a $ succMultiplication a (b - 1)

-- ghci> succMultiplication 2 3
-- 6

-- c) ridicarea la putere

succPower :: Integer -> Integer -> Integer
succPower a b | b == 1 = a
succPower a b          = succMultiplication a $ succPower a (b - 1)

-- ghci> succPower 2 3
-- 8
-- ghci> succPower 3 2
-- 9

-- //////////////////////////////////////////////////////////////

-- Exercitiul 9

-- a) mod operator

myMod :: Integer -> Integer -> Integer
myMod a b = myModAux a b 1

myModAux :: Integer -> Integer -> Integer -> Integer
myModAux a b n | a - b * n == b = 0
myModAux a b n | a - b * n < b  = a - b * n
myModAux a b n                  = myModAux a b (n + 1)

-- ghci> myMod 4 2
-- 0
-- ghci> myMod 5 2
-- 1
-- ghci> myMod 5 3
-- 2
-- ghci> myMod 10 3
-- 1
-- ghci> myMod 10 4
-- 2

-- b) div operator

myDiv :: Integer -> Integer -> Integer
myDiv a b = myDivAux a b 1

myDivAux :: Integer -> Integer -> Integer -> Integer
myDivAux a b n | b * n == a = n
myDivAux a b n | b * n > a = n - 1
myDivAux a b n             = myDivAux a b (n + 1)

-- ghci> myDiv 4 2
-- 2
-- ghci> myDiv 5 2
-- 2
-- ghci> 5 `div` 2  asa ar trebuii sa mearga
-- 2

-- //////////////////////////////////////////////////////////////

-- Bonus

-- Suma valorilor mai mari de dupa un element k care sunt mai mari decat acesta

sumOfGr (x:xs) k valueK | null xs && x > valueK && valueK /= (-1) = x
sumOfGr (x:xs) k valueK | null xs                                 = 0
sumOfGr (x:xs) k (-1)   | k <= 0                                  = sumOfGr xs (k - 1) x
sumOfGr (x:xs) k valueK | x > valueK && valueK /= (-1)            = x + sumOfGr xs (k - 1) valueK
sumOfGr (x:xs) k valueK                                           = sumOfGr xs (k - 1) valueK

-- ghci> sumOfGr [1, 3, 2, 4] 2 (-1)
-- 4
-- ghci> sumOfGr [1, 3, 2, 4] 1 (-1)
-- 4
-- ghci> sumOfGr [1, 3, 2, 4] 0 (-1)
-- 9
-- ghci> sumOfGr [1, 3, 2, 1] 0 (-1)
-- 5
-- ghci> sumOfGr [2, 3, 2, 1] 0 (-1)
-- 3

-- //////////////////////////////////////////////////////////////

-- Suma valorilor mai mari de dinaintea unui element k care sunt mai mari decat acesta

sumOfGrFrom0 (x:xs) k valueK | valueK == (-1) = sumOfGrFrom0 (x:xs) k (sumOfHelper (x:xs) k)
sumOfGrFrom0 (x:xs) k valueK | x > valueK     = x + sumOfGr xs k valueK
sumOfGrFrom0 (x:xs) k valueK                  = sumOfGr xs k valueK

sumOfHelper (x:xs) k | k <= 0 = x
sumOfHelper (x:xs) k          =  sumOfHelper xs (k - 1)

-- ghci> sumOfGrFrom0 [5, 4, 3] 0 (-1)
-- 0
-- ghci> sumOfGrFrom0 [5, 4, 3] 1 (-1)
-- 5
-- ghci> sumOfGrFrom0 [5, 4, 3] 2 (-1)
-- 9

