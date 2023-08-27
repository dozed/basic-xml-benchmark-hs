# basic-xml-benchmark-hs

Benchmark of several Haskell XML libraries against parsing [neurips2022.xml](https://github.com/dozed/basic-xml-benchmark-hs/blob/main/data/neurips2022.xml) and adding UUIDs to a subset of elements.

Example:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<bib>
  <publication uuid="ab929494-2cd9-4ba0-9762-2b92781a12be">
    <title>On Kernelized Multi-Armed Bandits with Constraints.</title>
    <author uuid="fb6536ec-6108-43dd-afc4-c66784a5361b">
      <name>Xingyu Zhou</name>
    </author>
    <author uuid="c263a753-c2e2-4429-99fb-9e78490d4331">
      <name>Bo Ji</name>
    </author>
    <url>http://papers.nips.cc/paper_files/paper/2022/hash/00295cede6e1600d344b5cd6d9fd4640-Abstract-Conference.html</url>
  </publication>
  <publication uuid="fa771a3c-ddfc-42bc-b91c-3ad305e3bc9e">
    <title>Fast Bayesian Coresets via Subsampling and Quasi-Newton Refinement.</title>
    <author uuid="d583ffce-73de-4939-8ac5-0ece79b297cf">
      <name>Cian Naik</name>
    </author>
    <author uuid="f665ec49-9e91-4c90-a810-37efb498eeed">
      <name>Judith Rousseau</name>
    </author>
    <author uuid="ea7f4a22-0e56-4656-a13d-9961ff247267">
      <name>Trevor Campbell</name>
    </author>
    <url>http://papers.nips.cc/paper_files/paper/2022/hash/005413e90d003d13886019607b037f52-Abstract-Conference.html</url>
  </publication>
</bib>
```

XML libraries:
- [xml 1.3.14](https://hackage.haskell.org/package/xml-1.3.14) 
- [hxt 9.3.1.22](https://hackage.haskell.org/package/hxt-9.3.1.22)
- [hexml 0.3.4](https://hackage.haskell.org/package/hexml-0.3.4)
- [xeno 0.6](https://hackage.haskell.org/package/xeno-0.6)
- [hexpat 0.20.13](https://hackage.haskell.org/package/hexpat-0.20.13)
- [xml-conduit 1.9.1.3](https://hackage.haskell.org/package/xml-conduit-1.9.1.3)
- [xml-nodestream](https://github.com/travisbrown/xml-nodestream)

# Results

![](benchmark.png)

```
benchmarking parse/xml
time                 432.9 ms   (364.6 ms .. 473.3 ms)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 412.6 ms   (390.5 ms .. 421.3 ms)
std dev              15.25 ms   (3.663 ms .. 20.17 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hxt
time                 956.0 ms   (844.8 ms .. 1.062 s)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 970.5 ms   (954.6 ms .. 997.5 ms)
std dev              25.76 ms   (537.4 μs .. 31.49 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hexml
time                 38.09 ms   (37.00 ms .. 39.33 ms)
                     0.996 R²   (0.988 R² .. 0.999 R²)
mean                 39.87 ms   (38.72 ms .. 41.86 ms)
std dev              3.184 ms   (1.543 ms .. 5.557 ms)
variance introduced by outliers: 32% (moderately inflated)

benchmarking parse/xeno
time                 50.06 ms   (48.96 ms .. 51.26 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 51.21 ms   (50.03 ms .. 54.44 ms)
std dev              3.622 ms   (1.307 ms .. 6.325 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking parse/hexpat (ByteString)
time                 175.5 ms   (165.0 ms .. 185.0 ms)
                     0.997 R²   (0.993 R² .. 1.000 R²)
mean                 182.0 ms   (177.4 ms .. 187.2 ms)
std dev              6.489 ms   (3.432 ms .. 8.834 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/hexpat (String)
time                 259.2 ms   (256.2 ms .. 262.1 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 266.6 ms   (263.3 ms .. 277.6 ms)
std dev              6.344 ms   (1.170 ms .. 8.791 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking parse/hexpat (Text)
time                 163.6 ms   (159.5 ms .. 169.6 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 168.5 ms   (165.4 ms .. 175.9 ms)
std dev              6.880 ms   (1.886 ms .. 9.921 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking parse/xml-conduit (DOM)
time                 202.5 ms   (180.7 ms .. 216.3 ms)
                     0.994 R²   (0.978 R² .. 1.000 R²)
mean                 201.6 ms   (196.4 ms .. 205.0 ms)
std dev              6.073 ms   (4.058 ms .. 8.569 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/xml-conduit (StreamDOM)
time                 225.7 ms   (221.9 ms .. 230.3 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 221.4 ms   (214.5 ms .. 223.9 ms)
std dev              5.826 ms   (358.0 μs .. 8.374 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking add-uuids/xml (DOM)
time                 471.7 ms   (364.7 ms .. 555.0 ms)
                     0.994 R²   (0.979 R² .. 1.000 R²)
mean                 500.0 ms   (480.3 ms .. 515.6 ms)
std dev              19.27 ms   (2.497 ms .. 23.96 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml (Cursor)
time                 461.8 ms   (453.4 ms .. 475.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 461.2 ms   (457.4 ms .. 463.8 ms)
std dev              4.323 ms   (2.093 ms .. 5.878 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hxt
time                 949.2 ms   (865.4 ms .. 1.038 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 969.2 ms   (952.3 ms .. 983.5 ms)
std dev              19.70 ms   (16.43 ms .. 22.20 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml-conduit (DOM)
time                 364.1 ms   (338.1 ms .. 419.4 ms)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 355.2 ms   (347.7 ms .. 361.5 ms)
std dev              8.021 ms   (3.638 ms .. 10.49 ms)
variance introduced by outliers: 19% (moderately inflated)
```
