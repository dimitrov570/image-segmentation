# Сегментиране на изображение
## Проект към курса по Функционално програмиране във ФМИ

Сегментиране на изображение с използване на K-means алгоритъма за клъстеризиране. Програмата получава като аргументи:
- име на файл в P3 текстов PPM формат, който да се обработва; 
- номер, който представлява броя на клъстерите;
- име на изходен файл, в който да се запише резултата от обработката (в същия формат);
- номер, който представлява сложността на алгоритъма (колко пъти да се пресмятат клъстерите; ако е <= 0 се извършва пресмятането докато не се получат клъстери, които повече не се променят).

Изображение ще представяме чрез неговите размери и списък от списъци от пикселите му. В този списък, всеки подсписък представя един ред на изображението. Пръв в списъка е най-горният ред, следван от втория и т.н. Всеки ред се представя като списък от цветовите стойности за всеки пиксел, започвайки от най-левия и продължавайки към най-десния.

```haskell
data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Pixel]] } deriving (Show,Read)
```

Пиксел ще представяме чрез следния тип:

```haskell
data Pixel = Pixel { color     :: Rgb
                   , cluster   :: Rgb
                   , distance  :: Double } deriving (Show,Read,Eq,Ord)
```

където cluster представлява центъра на клъстера, към който принадлежи пиксела, а distance е разстоянието между пиксела и центъра на клъстера. Под разстояние между два пиксела, които имат (r1, g1, b1) и (r2, g2, b2) като стойности за цветове подразбираме Евклидово разстояние между цветове, дефинирано по следния начин:

distance := sqrt((r2-r1)^2+(g2-g1)^2+(b2-b1)^2)

а да се спести време от пресмятането на квадратния корен се съхранява distance^2.

Цвят ще представяме чрез следния тип:

```haskell
data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show,Read,Eq,Ord)
```

Изображението се прочита от входния файл и се съхранява като Image.
След приключването на алгоритъма полученото изображение се записва във файл, чието име е подадено като аргумент на програмата.
