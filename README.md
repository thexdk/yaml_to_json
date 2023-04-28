Программа преобразует YAML-объект в JSON-объект.

При запуске программы в аргументах командной строки нужно указать сначала имя входного файла, затем (опционально) имя выходного файла, 
остальные аргументы игнорируются.
Программа формирует файл с указанным названием (если имя не указано -- jsoned.txt), в котором содержится либо полученный json-объект, либо сообщение об ошибке.

На вход программа требует файл с YAML-объектом, причем с некоторыми дополнительными требованиями:
  * Каждый уровень вложенности объектов идентифицируется двумя пробелами. Иначе программа будет выводить сообщение "Bad indentation!"
  * Списки как значения для данного уровня вложенности записываются через черту с тем же отступом, что и ключ. 
  * Если элементом списка является объект, то сначала пишется черта, потом, начиная со следующей строки, объект с отступом, соответствующим данному уровню вложенности
  
Еще один возможный вариант ошибки -- "Something extraneous!". Он свидетельствует об отсутствии на некоторой строке, не относящейся к списку, 
двоеточия, определяющего пару ключ-значение.

Программа формирует корректный json-объект: каждый вложенный объект обернут в фигурные скобки, записи разделяются запятыми, строки заключаются в кавычки, допускаются пустые значения, списки записываюся в квадратных скобках по одному значению на строку, в качестве элементов допускаются объекты.

 В директории test можно найти несколько примеров входных файлов и результатов работы для них.

