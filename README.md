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
- [hexpat-lens 0.1.9](https://hackage.haskell.org/package/hexpat-lens-0.1.9)
- [xml-conduit 1.9.1.3](https://hackage.haskell.org/package/xml-conduit-1.9.1.3)
- [xml-nodestream](https://github.com/travisbrown/xml-nodestream)
- [xml-lens 0.3.1](https://hackage.haskell.org/package/xml-lens-0.3.1)


# Results

![](benchmark.png)

```
benchmarking parse/xml
time                 468.6 ms   (326.3 ms .. 536.6 ms)
                     0.989 R²   (0.984 R² .. 1.000 R²)
mean                 508.8 ms   (473.1 ms .. 569.5 ms)
std dev              58.94 ms   (12.69 ms .. 77.41 ms)
variance introduced by outliers: 23% (moderately inflated)

benchmarking parse/hxt
time                 1.085 s    (856.5 ms .. 1.343 s)
                     0.993 R²   (0.976 R² .. 1.000 R²)
mean                 1.111 s    (1.044 s .. 1.134 s)
std dev              45.68 ms   (9.411 ms .. 56.91 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hexml
time                 40.58 ms   (38.46 ms .. 42.80 ms)
                     0.994 R²   (0.988 R² .. 0.999 R²)
mean                 42.11 ms   (41.07 ms .. 44.65 ms)
std dev              3.169 ms   (1.424 ms .. 5.103 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking parse/xeno
time                 51.60 ms   (50.95 ms .. 52.35 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 53.53 ms   (52.69 ms .. 54.83 ms)
std dev              2.138 ms   (981.8 μs .. 3.439 ms)

benchmarking parse/hexpat (ByteString)
time                 182.1 ms   (179.7 ms .. 187.2 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 187.5 ms   (184.4 ms .. 193.2 ms)
std dev              6.432 ms   (1.339 ms .. 9.300 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/hexpat (String)
time                 270.5 ms   (261.9 ms .. 292.4 ms)
                     0.998 R²   (0.989 R² .. 1.000 R²)
mean                 267.9 ms   (265.0 ms .. 272.9 ms)
std dev              5.160 ms   (675.7 μs .. 6.987 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking parse/hexpat (Text)
time                 190.9 ms   (167.8 ms .. 214.8 ms)
                     0.994 R²   (0.989 R² .. 1.000 R²)
mean                 184.5 ms   (177.0 ms .. 193.8 ms)
std dev              12.19 ms   (7.516 ms .. 17.47 ms)
variance introduced by outliers: 15% (moderately inflated)

benchmarking parse/xml-conduit (DOM)
time                 213.6 ms   (207.0 ms .. 218.2 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 210.7 ms   (208.5 ms .. 212.9 ms)
std dev              3.163 ms   (1.944 ms .. 4.491 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/xml-conduit (StreamDOM)
time                 237.2 ms   (229.2 ms .. 243.2 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 234.0 ms   (230.8 ms .. 236.5 ms)
std dev              3.729 ms   (1.362 ms .. 4.639 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking add-uuids/xml (DOM)
time                 578.5 ms   (498.9 ms .. 647.4 ms)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 524.8 ms   (499.0 ms .. 539.4 ms)
std dev              29.89 ms   (25.53 ms .. 31.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml (Cursor)
time                 482.0 ms   (435.0 ms .. 520.7 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 489.0 ms   (482.3 ms .. 494.7 ms)
std dev              7.038 ms   (2.153 ms .. 8.928 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hxt
time                 993.9 ms   (857.6 ms .. NaN s)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 1.021 s    (980.7 ms .. 1.037 s)
std dev              29.32 ms   (12.31 ms .. 36.09 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hexpat (Lens)
time                 990.8 ms   (964.7 ms .. 1.039 s)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 1.029 s    (1.010 s .. 1.061 s)
std dev              30.48 ms   (4.860 ms .. 39.37 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml-conduit (DOM)
time                 297.9 ms   (270.3 ms .. 316.2 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 312.2 ms   (301.1 ms .. 331.2 ms)
std dev              20.27 ms   (2.452 ms .. 27.52 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking add-uuids/xml-conduit (Lens)
time                 374.0 ms   (337.5 ms .. 444.3 ms)
                     0.996 R²   (0.991 R² .. 1.000 R²)
mean                 372.1 ms   (360.3 ms .. 379.4 ms)
std dev              11.57 ms   (5.067 ms .. 14.74 ms)
variance introduced by outliers: 19% (moderately inflated)
```
