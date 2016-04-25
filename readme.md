### CSV to XML Conversion

Building with Stack:

```
stack build && cat website_.csv | stack exec ma-csv-xml-exe -- csv pipe
stack build && stack exec ma-csv-xml-exe -- csv io --input ./Website_.csv --output=./Website2_.xml
```


Using the OSX build version:

```
cat Website_.csv | ma-csv-xml-exe csv pipe
```

### XML to CSV Conversion

```
stack build && stack exec ma-csv-xml-exe -- xml io --input ./Website_.xml --output=./Website2_.csv
stack build && cat Website_.xml | stack exec ma-csv-xml-exe -- xml pipe
```

Using the OSX build version:

```
cat Website_.xml | ma-csv-xml-exe xml pipe
```
