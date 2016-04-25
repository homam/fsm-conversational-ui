### CSV to XML Conversion
```
stack build && cat website_.csv | stack exec ma-csv-xml-exe -- csv pipe
stack build && stack exec ma-csv-xml-exe -- csv io --input ./Website_.csv --output=./Website2_.xml
```
