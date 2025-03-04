//На основе списка вещественных чисел получить список из их последних цифр. 
//Подсказка. Преобразовать число в строку, выделить последний символ, 
//преобразовать его обратно в число.

open System
open System.IO

// Парсинг с выводом ошибок
let parseChar s =
    try
        Some (char s)
    with
    | :? System.FormatException ->
        printfn "'%s' не допустимый формат символа" s
        None
    | ex ->
        printfn "ошибка: %s" ex.Message
        None

// Получение списка float с клавиатуры
let readChars mess =
    Console.Clear()
    printf "%s >> " mess
    let line = Console.ReadLine()

    if line = "" then
        Some []
    else
        let nums =
            // получение списка чисел из строки
            line.Split(' ')
            |> List.ofArray
            |> List.map parseChar

        // отработка ошибки при парсинге
        // список без None станет содержать просто float
        if (List.contains None nums) then
            None
        else
            Some (List.choose id nums)

let readCharsFromFile filePath =
    Console.Clear()
    try
        let lines = File.ReadAllLines(filePath)
        if lines = [||] then
            Some []
        else
            let nums = 
                List.ofArray lines
                |> List.map parseChar

            // отработка ошибки при парсинге
            // список без None станет содержать просто float
            if (List.contains None nums) then
                None
            else
                Some (List.choose id nums)  
    with
    | :? FileNotFoundException -> failwith "Файл не найден"
    | :? IOException as ex -> failwith (sprintf "Ошибка ввода-вывода: %s" ex.Message)
    

// Ввод числа int >= 0
let rec readSize mess =
    printf "%s >> " mess
    let s = Console.ReadLine()
    try
        let num = int s
        if num >= 0 then
            Some num
        else
            printfn "число '%s' не допустимый размер, возможные: 0 1 2 3 4..." s
            None
    with
        | :? System.FormatException ->
            printfn "'%s' не допустимый формат числа" s
            None
        | :? System.OverflowException ->
            printfn "число '%s' слишком большое" s
            None
        | ex ->
            printfn "ошибка: %s" ex.Message
            None

// список случайных вещественных чисел
let rec getRandChars n = 
    if n <= 0 then
        []
    else
        // в диапазоне печатных символов ASCII
        char (Random().Next(32, 126)) :: getRandChars (n-1)

let randomGen () =
    Console.Clear()
    let num =  readSize "Введите кол-во случайных символов в списке"
    if num <> None then
        Some (getRandChars (Option.get num))
    else
        None

let rec getNums (mess) =
    Console.Clear()
    printfn "%s" mess
    printfn "   [1] Ввести с клавиатуры"
    printfn "   [2] Сгенерировать случайно"
    printfn "   [3] Из файла"
    printfn "   [Esc] <- назад"

    match Console.ReadKey(true).Key with
    | ConsoleKey.D1 -> readChars "введите список символов через пробел"
    | ConsoleKey.D2 -> randomGen ()
    | ConsoleKey.D3 -> readCharsFromFile "test.txt"
    | ConsoleKey.Escape -> None
    | _ -> getNums (mess)

let getLastDigit num =
    let s = num.ToString()
    int ( (s.[s.Length-1]).ToString() )

let rec main () =
    let listOfChars = getNums ("Выберете метод получения списка символов")

    if listOfChars <> None then
        let SomeListOfChars = Option.get listOfChars
        Console.Clear()
        printfn "Получен список %A" SomeListOfChars
        printfn "Объединение в строку: %s" (List.fold (+) "" (List.map string SomeListOfChars))

    // выход
    printf      "Любую клавишу для повторного ввода списка, для выхода Esc..."
    if (Console.ReadKey(true).Key <> ConsoleKey.Escape) then
        printfn "\r                                                            "
        main ()
    else
        printfn "\rПрограмма приостановлена                                    "


// =========== ТОЧКА ВХОДА ===========
main ()
