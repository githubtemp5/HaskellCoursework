 -- MATHFUN
-- 826133
--
import Data.List
import Text.Printf
import Data.Char
--
-- Types
type Rating = (Title, Opinion)
data Opinion = Null|Like|Dislike
    deriving (Eq, Show)
    
-- Define Film type here 
type User = String
type Title = String
type Director = String
type Year = Int
type UsersWhoLike = [User]
type UsersWhoDislike = [User]
type Film = (Title, Director, Year, UsersWhoLike, UsersWhoDislike)
    
testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Dave"], ["Sam", "Olga", "Tim"]),
 ("The Fly", "David Cronenberg", 1986, ["Garry", "Dave", "Zoe"], ["Kevin", "Emma", "Heidi", "Jo", "Kate"]),
 ("Body Of Lies", "Ridley Scott", 2008, ["Garry", "Dave"], ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
 ("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"], ["Olga", "Tim", "Zoe", "Paula"]),
 ("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"], ["Sam", "Wally", "Kate"]),
 ("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"], ["Olga", "Dave", "Kate", "Zoe"]),
 ("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"], ["Tim", "Emma", "Jo", "Olga"]),
 ("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"], ["Tim", "Garry", "Ian", "Neal"]),
 ("Alien: Covenant", "Ridley Scott", 2017, ["Kevin", "Tim"], ["Emma", "Jo", "Liz"]),
 ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"], ["Jenny", "Kate", "Emma", "Olga"]),
 ("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"], ["Bill", "Garry", "Ian", "Kate"]),
 ("Jaws", "Steven Spielberg", 1975, ["Jenny", "Emma", "Bill", "Neal"], ["Sam", "Ian", "Kate"]),
 ("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"], ["Ian", "Neal", "Tim", "Liz"]),
 ("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"], ["Neal"]),
 ("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe", "Heidi"], ["Jo"]),
 ("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Garry"], ["Heidi", "Bill", "Sam", "Zoe"]),
 ("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"], ["Kate", "Jenny", "Zoe"]),
 ("True Lies", "James Cameron", 1994, ["Sam", "Dave"], ["Emma", "Olga", "Jenny", "Zoe"]),
 ("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"], ["Wally", "Dave", "Jenny", "Zoe"]),
 ("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Jenny", "Zoe"], ["Olga", "Heidi"]),
 ("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"], ["Heidi", "Jenny", "Sam"]),
 ("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Jo"], ["Dave", "Olga"]),
 ("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"], ["Heidi"]),
 ("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz"], ["Olga", "Jo", "Neal"])]


--Functional Code

printfFormatString :: String
printfFormatString = "%-35s %-20s %-15s %10.0f\n"

--Question i
addFilm :: String -> String -> Int -> [Film] -> [Film]
addFilm title dir year database = (title, dir, year, [], []): database

--Question ii -HAS TO BE STRING
filmsAsString:: [Film] -> String
filmsAsString (film:films) = printf printfFormatString
                                    (getTitle film) (getDirector film) (show(getReleaseDate film)) (getRating film) ++ (filmsAsString films)
filmsAsString _ = ""

getReleaseDate :: Film -> Int
getReleaseDate (_, _, yyyy, _, _) = yyyy

getTitle :: Film -> String
getTitle (title, _, _, _, _) = title

--Question iii -- HAS TO BE STRING
filmsByDirector :: String -> [Film] -> String
filmsByDirector dir database = foldl (++) 
                                        ""
                                        [printf printfFormatString (getTitle film) (getDirector film) (show(getReleaseDate film)) (getRating film) | film<-(filmsByPartcrDir dir database)]

filmsByPartcrDir :: String -> [Film] -> [Film]
filmsByPartcrDir dir database = [film | film <- database, toLowerString dir == (toLowerString.getDirector) film]

getDirector :: Film -> String
getDirector (_, director, _, _, _) = director

toLowerString :: String -> String
toLowerString str = [toLower c | c <- str]
--Question iv -- HAS TO BE STRING

websiteRating75 :: [Film] -> String ---HAS TO BE STRING
websiteRating75 database = foldl (++) ""
                                        [printf printfFormatString (getTitle film) (getDirector film) (show(getReleaseDate film)) (getRating film) | film <- database, getRating film >= 75]

getRating :: Film -> Double
getRating film
    |isNaN ((100 * getLikesCount film)/(getLikesCount film + getDislikesCount film)) = 0
    |otherwise = (100 * getLikesCount film)/(getLikesCount film + getDislikesCount film)

--gets the number of users who liked the film    
getLikesCount :: Film -> Double
getLikesCount (_, _, _, likes, _) = (fromIntegral.length) likes

getDislikesCount :: Film -> Double
getDislikesCount (_, _, _, _, dislikes) = (fromIntegral.length) dislikes 

--Question v
averageRating :: String -> [Film]-> Double
averageRating dir db = (foldr (+) 0 (map (getRating) (filmsByPartcrDir dir db)))/ ((fromIntegral.length) (filmsByPartcrDir dir db))

--Question vi
userOpinion :: String -> [Film] -> String
userOpinion user database = foldl (++) 
                                       (printf "%-30s%-30s\n\n" "Film Title" "User Opinion")
                                       [printf "%-30s%-30s\n" (getTitle film) (show (checkLikeDislike user film)) | film <- userRatedFilms user database]

--returns the list of films a user has rated                                       
userRatedFilms :: String ->[Film]-> [Film]
userRatedFilms user database = [film | film <-database, elem user (getLikeUsers film) || elem user (getDislikeUsers film)]

checkLikeDislike :: String -> Film -> Opinion
checkLikeDislike user film
    |elem user (getLikeUsers film) = Like
    |otherwise = Dislike

--gets the list of users who liked the given film 
getLikeUsers :: Film -> [String]
getLikeUsers (_, _, _, likes, _) = likes

getDislikeUsers :: Film -> [String]
getDislikeUsers (_, _, _, _, dislikes) = dislikes

--Question vii

addOpinion :: String -> Film-> Opinion -> [Film] -> [Film]
addOpinion user (title, dir, yyyy, uLike, dLike) Like db = addFilmOp title dir yyyy (checkLikeAddOp uLike  user) (rmvOpinion user dLike) (rmvFilm title db)
addOpinion user (title, dir, yyyy, uLike, dLike) Dislike db = addFilmOp title dir yyyy (rmvOpinion user uLike) (checkDislikeAddOp dLike  user) (rmvFilm title db)

addFilmOp :: String -> String -> Int -> [String] -> [String] ->[Film] -> [Film]
addFilmOp title dir year l d database = (title, dir, year, l, d): database

rmvFilm :: String -> [Film] -> [Film]
rmvFilm title db = [film | film <- db, not(getTitle film == title)]

--checks if user has liked the film before and returns the list of users who liked the film, adds to the list the user hasn't liked
checkLikeAddOp :: [String] -> String -> [String]
checkLikeAddOp uLike user
    |elem user uLike = uLike
    |otherwise = user:uLike

--removes the user from a list of user opinions
rmvOpinion :: String -> [String]-> [String]
rmvOpinion _ [] = []
rmvOpinion user (current:rest)
    |user == current = rmvOpinion user rest
    |otherwise = current: rmvOpinion user rest
    
checkDislikeAddOp :: [String] -> String -> [String]
checkDislikeAddOp uDislike user
    |elem user uDislike = uDislike
    |otherwise = user:uDislike

--Question viii -- HAS TO BE STRING

filmsBetween :: Int -> Int -> [Film] -> String
filmsBetween start end database = foldl (++) 
                                            "" 
                                            [printf printfFormatString (getTitle film) (getDirector film) (show(getReleaseDate film)) (getRating film) | film <- sortDescending
                                            [film | film <- database, getReleaseDate film >= start && getReleaseDate film <=end]]

sortDescending :: [Film]-> [Film]
sortDescending filmsWithRating = (rearrange.reverse.sort)
                                [(getRating(title,dir,yyyy,likes,disLikes), title, dir, yyyy, likes, disLikes) | (title, dir, yyyy, likes, disLikes) <-filmsWithRating]

--rearranges the given                                 
rearrange :: [(Double, String, String, Int, [String], [String])] -> [Film]
rearrange disarranged = [(title, dir, yyyy, likes, disLikes) | (webRating, title, dir, yyyy, likes, disLikes)<- disarranged]

--Checks whether a given String is made up of alphabets and spaces only
isString :: String -> Bool
isString str = foldr (&&) True [isLetter c||isSpace c | c <- str]

--Checks whether the input is a number
isValidNumber :: String -> Bool
isValidNumber str = (foldr (&&) True [isNumber c | c <- str])

--checks whether a director already exists
directorAlreadyExists :: String -> [Film] -> Bool
directorAlreadyExists inputDir database = foldl (||) False [(toLowerString.getDirector) film == toLowerString inputDir| film <- database]

--returns the type Film given the title of it
getFilmByTitle :: String -> [Film] -> Film
getFilmByTitle title database = (last[film | film <- database, (getTitle film) == title])

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO()
demo 1 = do
    displayFilmColumns
    putStrLn (filmsAsString $ addFilm "Sherlock Gnomes" "John Stevenson" 2018 testDatabase)
demo 2 = do
    displayFilmColumns
    putStrLn (filmsAsString testDatabase)
demo 3 = do
    displayFilmColumns
    putStrLn (filmsByDirector "Ridley Scott" testDatabase)
demo 4 = do
    putStrLn (websiteRating75 testDatabase)
demo 5 = do
    putStrLn (printf "%.1f" (averageRating "Ridley Scott" testDatabase)) 
demo 6 = do
    putStrLn (userOpinion "Emma" testDatabase)
demo 7 = do
    displayFilmColumns
    putStrLn (filmsAsString $ addOpinion "Emma" (getFilmByTitle "Avatar" testDatabase) Like testDatabase)
demo 71 = do
    displayFilmColumns
    putStrLn (filmsAsString $ addOpinion "Emma" (getFilmByTitle "Titanic" testDatabase) Like testDatabase)
demo 72 = do
    displayFilmColumns
    putStrLn (filmsAsString $ addOpinion "Emma" (getFilmByTitle "Jaws" testDatabase) Dislike testDatabase)
demo 8 = do
    displayFilmColumns
    putStrLn (filmsBetween 2000 2006 testDatabase)

--
--
-- Your user interface code goes here
--
--USER INTERFACE CODE
main :: IO()
main = do
    originalFilePath <- return ("database.txt")
    fileContents <- readFile originalFilePath
    database <- convertRawDataToFilmList fileContents

    displayFilmColumns
    putStr (filmsAsString database)

    user <- getStringInput("Enter your name: ")
    putStrLn ("Hi, "++ user)
    newFile <- executeChoice user database
    
    if(database == newFile) then do
        putStrLn ("BYE")
    else do
        putStrLn "Updating Changes if any.."
        writeFile originalFilePath (show newFile)
        putStrLn "BYE"

--displays the heading columns
displayFilmColumns :: IO()
displayFilmColumns = do
    printf "%-35s%-20s%-15s%10s\n\n" "Title" "Director" "Release Date" "Website Rating(%)"

executeChoice :: String -> [Film] -> IO [Film]
executeChoice user database = do
    displayMainMenu
    userMenuOption <- getUserMenuChoice

    if(userMenuOption == 0) then do
        return (database)

    else if(userMenuOption == 1) then do
        filmName <- getInput ("Enter the title of the Film: ")
        filmDirector <- getStringInput ("Enter the name of the director: ")
        releaseDate <- getIntInput ("Enter the year of release: ")
        newDatabase <- return (addFilm filmName filmDirector releaseDate database)
        putStrLn "Film added"
        executeChoice user newDatabase

    else if(userMenuOption == 2) then do
        displayFilmColumns
        putStr(filmsAsString database)
        executeChoice user database

    else if(userMenuOption == 3) then do
        dir <- getDirectorFromInput("Enter the name of the director: ") database
        displayFilmColumns
        putStrLn (filmsByDirector dir database)
        executeChoice user database

    else if(userMenuOption == 4) then do
        putStrLn "These websites have a rating of 75% or higher: "
        displayFilmColumns
        putStrLn (websiteRating75 database)
        executeChoice user database

    else if(userMenuOption == 5) then do
        dir <- getDirectorFromInput("Enter the full name of the director: ") database
        putStr ("The average rating for "++dir ++" is: ")
        putStr (printf "%.1f%%"(averageRating dir database))
        executeChoice user database

    else if(userMenuOption == 6) then do
        putStrLn (userOpinion user database)
        executeChoice user database

    else if(userMenuOption == 7) then do
        filmTitle <- getInput("Enter the title of the film: ")
        film <- return(getFilmByTitle filmTitle database)
        opinion <- inputOpinion
        newDB <- return (addOpinion user film opinion database)
        putStrLn "Changes made"
        executeChoice user newDB
        
    else do
        startYear <- getIntInput("Enter the start year: ")
        endYear <- getIntInput("Enter the end year: ")
        displayFilmColumns
        putStrLn(filmsBetween startYear endYear database)
        executeChoice user database

inputOpinion :: IO Opinion
inputOpinion = do
    userInput <- getStringInput("Enter 'like' or 'dislike': ")
    if(userInput=="like") then do
        return Like
    else if(userInput == "dislike") then do
        return Dislike
    else do
        putStrLn "Invalid opinion, please enter again: "
        inputOpinion

--gets the name of the director and it only returns the value if the director is in the database
getDirectorFromInput :: String -> [Film] -> IO String
getDirectorFromInput message database = do
    putStr message
    input <- getLine
    if(not(input=="")) then do
        if(directorAlreadyExists input database) then do
            return input
        else do
            putStrLn "No films for the given director exists in the database\n"
            getDirectorFromInput message database
    else do
        putStrLn "Invalid input"
        getDirectorFromInput message database

--converts the data from txt file to [Film] type
convertRawDataToFilmList :: String -> IO [Film]
convertRawDataToFilmList str = 
    return (read str :: [Film])


--displays the main menu
displayMainMenu :: IO()
displayMainMenu = do
    putStrLn ""
    putStrLn "MAIN MENU: "
    putStrLn "Enter [1] to add a new film to the database"
    putStrLn "Enter [2] to display all films in the database"
    putStrLn "Enter [3] to display all films by a director"
    putStrLn "Enter [4] to display all films with a website rating of 75% or higher"
    putStrLn "Enter [5] to display the average website rating for the films of the given director"
    putStrLn "Enter [6] to display all the films a user has rated and their opinion"
    putStrLn "Enter [7] to like or dislike a particular film"
    putStrLn "Enter [8] to display all films between two given years(inclusive), sorted in descending order of the website rating"
    putStrLn ""
    putStrLn "OR Enter [0] to exit the program"
    putStrLn ""


getUserMenuChoice :: IO Int
getUserMenuChoice = do
    menuChoice <- getLine
    if(not(menuChoice=="") && isValidNumber menuChoice) then do
        choiceInInt <- toInt menuChoice
        if(choiceInInt >= 0 && choiceInInt <= 8) then do
            return choiceInInt
        else do
            putStrLn "The options are between 0 and 8"
            putStr "Enter your choice again: "
            getUserMenuChoice
    else do
        putStrLn "Invalid option."
        putStrLn "Menu choice can only be numbers and between 0 and 8"
        putStr "Enter your choice again: "
        getUserMenuChoice

getStringInput :: String -> IO String
getStringInput message = do
    putStr message
    input <- getLine
    if(not(input=="") && isString input) then do
        return input
    else do
        putStrLn "Invalid input"
        putStrLn "Input has to be letters"
        getStringInput message

-- gets the input and returns it only if it is a positive number
getIntInput  :: String -> IO Int
getIntInput displayMessage = do
    putStr displayMessage
    str <- getLine   
    if(not(str=="") && isValidNumber str) then do
        return (read str :: Int)
    else do
        putStrLn "Invalid input"
        putStrLn "Input also has to be a positive number"
        getIntInput displayMessage

--gets a string input and returns it if not empty
getInput :: String -> IO String
getInput message = do
    putStr message
    input <- getLine
    if(not(input=="")) then do
        return input
    else do
        putStrLn "Invalid input"
        getStringInput message

--input to IO Int
toInt :: String -> IO Int
toInt str = do
    return (read str :: Int)