# АНАЛИЗ ФАКТОРОВ, ВЛИЯЮЩИХ НА СРЕДНЮЮ ПРОДОЛЖИТЕЛЬНОСТЬ РАБОЧЕЙ НЕДЕЛИ

## Данные
Для исследования были выбраны данные индивидуального уровня 31 волны РМЭЗ НИУ ВШЭ (т.е. ответы респондентов представлены по состоянию на 2022 год). Модель выборки этой базы данных — «повторяющаяся выборка» с «разделяющейся панелью», домохозяйства и индивиды выбирались с использованием метода многоступенчатого территориального вероятностного выбора: из 2029 объединённых районов, которые затем были сгруппированы в 38 страт на основании географических факторов, этнической составляющей и уровня урбанизации. В выборке отсутствуют отдалённые и малонаселённые районы.

### Переменные
В изначальном датасете представлены ответы на 741 вопрос от 17 343 индивидов, из них были отобраны 16 переменных:
		
**Описание работы и должности**
* Средняя продолжительность рабочей недели
* Отрасль работы
* Среднемесячная зарплата (в рублях)
* Является ли производство вредным/опасным
* Использовался ли за последние 12 месяцев интернет для работы
* Наличие подчиненных
* Количество подчиненных
		
**Описание респондента**
* Пол респондента
* Возраст респондента (в годах)
* Образование респондента
* Трудовой стаж респондента (в годах)
		
**Описание места работы**
* Является ли место работы предприятием
* Является ли занятость официальной
* Сколько человек работает на предприятии
* Является ли государство владельцем / совладельцем предприятия
* Является ли респондент владельцем / совладельцем предприятия