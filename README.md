# basic-xml-benchmark-hs

Benchmark of several Haskell XML libraries against parsing the following structure:

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

Results:

![](benchmark.png)

```
benchmarking tp/xml
time                 414.5 ms   (367.2 ms .. 468.4 ms)
                     0.998 R²   (0.993 R² .. 1.000 R²)
mean                 415.0 ms   (397.5 ms .. 422.8 ms)
std dev              13.06 ms   (4.494 ms .. 17.53 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking tp/hxt
time                 978.3 ms   (888.5 ms .. 1.061 s)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 981.4 ms   (966.1 ms .. 990.5 ms)
std dev              15.23 ms   (6.670 ms .. 21.05 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking tp/hexml
time                 38.20 ms   (37.16 ms .. 39.62 ms)
                     0.997 R²   (0.993 R² .. 0.999 R²)
mean                 40.45 ms   (39.37 ms .. 42.82 ms)
std dev              2.940 ms   (1.732 ms .. 4.122 ms)
variance introduced by outliers: 25% (moderately inflated)

benchmarking tp/xeno
time                 50.68 ms   (49.28 ms .. 52.52 ms)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 52.18 ms   (51.30 ms .. 53.65 ms)
std dev              2.222 ms   (1.566 ms .. 3.459 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking tp/hexpat (ByteString)
time                 168.5 ms   (159.4 ms .. 176.9 ms)
                     0.997 R²   (0.991 R² .. 1.000 R²)
mean                 168.4 ms   (165.3 ms .. 170.9 ms)
std dev              4.240 ms   (2.861 ms .. 5.937 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking tp/hexpat (String)
time                 406.4 ms   (256.3 ms .. 661.4 ms)
                     0.900 R²   (0.854 R² .. 1.000 R²)
mean                 291.1 ms   (257.3 ms .. 353.4 ms)
std dev              63.95 ms   (10.10 ms .. 86.12 ms)
variance introduced by outliers: 58% (severely inflated)

benchmarking tp/hexpat (Text)
time                 168.4 ms   (162.4 ms .. 177.6 ms)
                     0.995 R²   (0.973 R² .. 1.000 R²)
mean                 171.5 ms   (166.2 ms .. 178.1 ms)
std dev              9.389 ms   (3.658 ms .. 11.86 ms)
variance introduced by outliers: 13% (moderately inflated)

benchmarking tp/xml-conduit (DOM)
time                 212.0 ms   (205.7 ms .. 217.6 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 204.4 ms   (200.2 ms .. 207.8 ms)
std dev              5.064 ms   (3.740 ms .. 6.383 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking tp/xml-conduit (StreamDOM)
time                 226.8 ms   (217.3 ms .. 235.4 ms)
                     0.999 R²   (0.995 R² .. 1.000 R²)
mean                 221.2 ms   (213.8 ms .. 225.8 ms)
std dev              7.590 ms   (3.657 ms .. 11.19 ms)
variance introduced by outliers: 14% (moderately inflated)
```
