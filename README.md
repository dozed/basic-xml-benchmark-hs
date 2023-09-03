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
time                 467.1 ms   (416.2 ms .. 536.4 ms)
                     0.998 R²   (0.992 R² .. 1.000 R²)
mean                 492.7 ms   (475.4 ms .. 512.3 ms)
std dev              23.23 ms   (325.4 μs .. 28.52 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hxt
time                 1.025 s    (999.7 ms .. 1.057 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.050 s    (1.037 s .. 1.070 s)
std dev              18.57 ms   (5.727 ms .. 24.21 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking parse/hexml
time                 40.89 ms   (39.98 ms .. 42.43 ms)
                     0.996 R²   (0.989 R² .. 0.999 R²)
mean                 40.63 ms   (40.01 ms .. 41.62 ms)
std dev              1.703 ms   (1.081 ms .. 2.454 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking parse/xeno
time                 52.57 ms   (50.17 ms .. 55.06 ms)
                     0.995 R²   (0.990 R² .. 0.999 R²)
mean                 54.33 ms   (52.97 ms .. 55.52 ms)
std dev              2.271 ms   (1.464 ms .. 3.518 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/hexpat (ByteString)
time                 168.6 ms   (151.7 ms .. 187.3 ms)
                     0.995 R²   (0.986 R² .. 1.000 R²)
mean                 181.6 ms   (174.1 ms .. 186.1 ms)
std dev              7.467 ms   (5.207 ms .. 9.081 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/hexpat (String)
time                 263.1 ms   (257.5 ms .. 270.9 ms)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 266.8 ms   (262.9 ms .. 272.3 ms)
std dev              5.934 ms   (2.959 ms .. 8.609 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking parse/hexpat (Text)
time                 193.1 ms   (176.7 ms .. 215.0 ms)
                     0.995 R²   (0.993 R² .. 1.000 R²)
mean                 181.8 ms   (178.2 ms .. 187.5 ms)
std dev              6.659 ms   (3.590 ms .. 11.35 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/xml-conduit (DOM)
time                 217.4 ms   (207.2 ms .. 228.2 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 210.2 ms   (204.3 ms .. 215.4 ms)
std dev              7.852 ms   (4.763 ms .. 10.87 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking parse/xml-conduit (StreamDOM)
time                 239.7 ms   (235.0 ms .. 244.8 ms)
                     1.000 R²   (0.998 R² .. 1.000 R²)
mean                 228.6 ms   (218.1 ms .. 232.7 ms)
std dev              8.353 ms   (3.788 ms .. 12.07 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking add-uuids/xml (DOM)
time                 526.1 ms   (459.9 ms .. 587.5 ms)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 543.3 ms   (531.6 ms .. 555.0 ms)
std dev              13.91 ms   (10.92 ms .. 16.78 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml (Cursor)
time                 486.0 ms   (449.8 ms .. 502.2 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 486.1 ms   (479.4 ms .. 492.6 ms)
std dev              6.839 ms   (2.519 ms .. 8.855 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hxt
time                 1.013 s    (863.8 ms .. 1.160 s)
                     0.997 R²   (0.989 R² .. 1.000 R²)
mean                 1.022 s    (996.3 ms .. 1.038 s)
std dev              25.95 ms   (8.690 ms .. 35.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/hexpat (Lens)
time                 1.004 s    (888.5 ms .. 1.165 s)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 1.024 s    (992.8 ms .. 1.043 s)
std dev              32.10 ms   (15.84 ms .. 45.01 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking add-uuids/xml-conduit (DOM)
time                 298.7 ms   (290.0 ms .. 307.0 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 311.1 ms   (306.4 ms .. 322.1 ms)
std dev              8.721 ms   (1.546 ms .. 11.96 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking add-uuids/xml-conduit (Lens)
time                 369.0 ms   (341.4 ms .. 426.1 ms)
                     0.997 R²   (0.995 R² .. 1.000 R²)
mean                 378.6 ms   (365.1 ms .. 393.6 ms)
std dev              16.30 ms   (4.436 ms .. 21.88 ms)
variance introduced by outliers: 19% (moderately inflated)
```
