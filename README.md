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
time                 483.2 ms   (421.0 ms .. 561.9 ms)
                     0.997 R²   (0.990 R² .. 1.000 R²)
mean                 485.1 ms   (466.6 ms .. 497.1 ms)
std dev              17.87 ms   (8.723 ms .. 23.47 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hxt
time                 1.122 s    (952.2 ms .. 1.238 s)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 1.103 s    (1.072 s .. 1.134 s)
std dev              38.46 ms   (22.49 ms .. 49.47 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hexml
time                 40.62 ms   (38.46 ms .. 42.96 ms)
                     0.992 R²   (0.984 R² .. 0.997 R²)
mean                 43.08 ms   (41.51 ms .. 46.28 ms)
std dev              4.279 ms   (1.907 ms .. 7.302 ms)
variance introduced by outliers: 34% (moderately inflated)

benchmarking parse/xeno
time                 51.77 ms   (49.95 ms .. 53.16 ms)
                     0.997 R²   (0.992 R² .. 0.999 R²)
mean                 54.22 ms   (53.06 ms .. 57.49 ms)
std dev              3.884 ms   (1.601 ms .. 6.369 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking parse/hexpat (ByteString)
time                 184.7 ms   (172.4 ms .. 196.0 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 176.6 ms   (173.5 ms .. 180.4 ms)
std dev              5.627 ms   (4.630 ms .. 6.858 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking parse/hexpat (String)
time                 260.4 ms   (242.3 ms .. 274.5 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 265.7 ms   (262.2 ms .. 268.1 ms)
std dev              3.889 ms   (2.236 ms .. 5.961 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking parse/hexpat (Text)
time                 176.0 ms   (158.0 ms .. 211.4 ms)
                     0.986 R²   (0.962 R² .. 1.000 R²)
mean                 183.8 ms   (174.7 ms .. 200.5 ms)
std dev              16.93 ms   (8.041 ms .. 23.22 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking parse/xml-conduit (DOM)
time                 211.4 ms   (205.2 ms .. 224.6 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 211.0 ms   (203.7 ms .. 215.1 ms)
std dev              7.078 ms   (2.534 ms .. 10.61 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/xml-conduit (StreamDOM)
time                 243.9 ms   (234.8 ms .. 252.7 ms)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 232.6 ms   (226.5 ms .. 237.6 ms)
std dev              8.137 ms   (5.880 ms .. 10.64 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking add-uuids/xml (DOM)
time                 494.1 ms   (405.9 ms .. 537.1 ms)
                     0.996 R²   (0.990 R² .. 1.000 R²)
mean                 507.2 ms   (494.7 ms .. 515.4 ms)
std dev              12.73 ms   (4.601 ms .. 16.66 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml (Cursor)
time                 480.7 ms   (466.9 ms .. 508.4 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 468.2 ms   (464.4 ms .. 474.6 ms)
std dev              6.406 ms   (2.356 ms .. 8.753 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hxt
time                 987.6 ms   (895.6 ms .. NaN s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 1.017 s    (999.7 ms .. 1.035 s)
std dev              22.06 ms   (10.05 ms .. 28.51 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hexpat (Lens)
time                 1.019 s    (845.3 ms .. 1.158 s)
                     0.997 R²   (0.988 R² .. 1.000 R²)
mean                 1.051 s    (1.016 s .. 1.077 s)
std dev              34.05 ms   (12.37 ms .. 46.68 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml-conduit (DOM)
time                 300.3 ms   (293.3 ms .. 306.7 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 304.8 ms   (301.0 ms .. 314.1 ms)
std dev              7.385 ms   (1.337 ms .. 9.753 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking add-uuids/xml-conduit (Lens)
time                 380.9 ms   (373.0 ms .. 398.8 ms)
                     1.000 R²   (0.999 R² .. NaN R²)
mean                 377.5 ms   (375.2 ms .. 379.6 ms)
std dev              2.658 ms   (552.0 μs .. 3.441 ms)
variance introduced by outliers: 19% (moderately inflated)
```
