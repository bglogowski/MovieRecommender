## server.R
#
# Project_4_0147_bryanmg2_BryanGlogowski
#
# CS598: PSL - Fall, 2020
# Bryan Glogowski <bryanmg2@illinois.edu>
#

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures


library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)


set.seed(0147)
library(Matrix)
suppressPackageStartupMessages(suppressWarnings(library(recommenderlab)))



get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
}


get_movies = function(file_name = 'data/ml-1m/movies.dat', ratings) {
    
    movies <- readLines(file_name)
    movies <- strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
    movies <- matrix(unlist(movies), ncol = 3, byrow = TRUE)
    movies <- data.frame(movies, stringsAsFactors = FALSE)
    
    colnames(movies) <- c('MovieID', 'Title', 'Genres')
    
    
    movies$MovieID <- as.integer(movies$MovieID)
    movies$Title <- iconv(movies$Title, "latin1", "UTF-8")
    
    #small_image_url = "https://bryanmg2.shinyapps.io/project4/images/"
    
    #movies$image_url = sapply(movies$MovieID, 
    #                          function(x) paste0(small_image_url, x, '.jpg'))
    
    
    small_image_url = "https://liangfgithub.github.io/MovieImages/"
    movies$image_url = sapply(movies$MovieID, 
                              function(x) paste0(small_image_url, x, '.jpg?raw=true'))
    
    movies$Year = as.numeric(unlist(
        lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
    
    movies$Genres <- strsplit(movies$Genres, split = "|", fixed = TRUE, useBytes = TRUE)
    
    genres <- unique(unlist(movies$Genres))
    
    
    for (g in genres) {
        movies[[g]] <- vector(mode = "logical", length = length(movies$Title))
    }
    
    counter <- 1
    for (i in movies$Genres) {
        for (g in genres) {
            if (g %in% i) {
                movies[[g]][counter] <- TRUE
            }
        }
        counter <- counter + 1
    }
    
    movies <- movies %>%  filter(MovieID %in% ratings$MovieID)
    
    movies <- ratings %>% 
        group_by(MovieID) %>% 
        summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
        inner_join(movies, by = 'MovieID') %>% 
        filter(ratings_per_movie > 10)
    
    movies <- movies[order(-movies$ave_ratings),]
    
    return(movies)
}

get_ratings <- function(file_name = 'data/ml-1m/ratings.dat') {
    
    ratings <- read.csv(file_name,
                        sep = ':',
                        colClasses = c('integer', 'NULL'),
                        header = FALSE)
    
    colnames(ratings) <- c('UserID', 'MovieID', 'Rating', 'Timestamp')
    
    return(ratings)
}

get_users <- function(file_name = 'data/ml-1m/users.dat') {
    
    users <- read.csv(file_name,
                      sep = ':',
                      header = FALSE)
    
    users <- users[, -c(2,4,6,8)]
    
    colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
    
    return(users)
}


#users   <- get_users()
ratings <- get_ratings()
movies  <- get_movies(file_name = 'data/ml-1m/movies.dat', ratings)
ratings <- ratings %>%  filter(MovieID %in% movies$MovieID)
#users   <- users %>%  filter(UserID %in% ratings$UserID)

genres <- unique(unlist(movies$Genres))


allmovies  <- movies
allratings <- ratings[,1:3]






shinyServer(function(input, output, session) {
    

    
    output$genres <- renderUI({
        selectInput("genre", "Select Genre", selected = "Drama", choices = genres, multiple = FALSE)
    })
    
    #dataDownload <- reactive({
    #    movies <- movies[movies[[input$genre]]]
    #    print("Got here")
    #})
    
    # show the books to be rated
    output$ratings <- renderUI({

        num_movies <- 6 # movies per row

        
        if (!is.null(input$genre)) { 
            movies  <<- allmovies[allmovies[[input$genre]],][1:100, , drop = TRUE]
            ratings <<- allratings %>%  filter(MovieID %in% movies$MovieID)
        }
        
        
        num_rows <- floor(length(movies$MovieID) / num_movies)
        
        if(num_rows > 50) {
            num_rows <- 50
        }
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                         div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btn, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            # hide the rating container
            useShinyjs()
            jsCode <- "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)
            

            # get the user's rating data
            value_list <- reactiveValuesToList(input)
            user_ratings <- get_user_ratings(value_list)
            #print("1")
            
            user_df <- cbind(UserID=as.character(rep(0, nrow(user_ratings))), user_ratings)
            #print("2")

            train    <- rbind(user_df, ratings)
            #print("3")
            
            i <- paste0('u', train$UserID)
            j <- paste0('m', train$MovieID)
            x <- train$Rating
            #print("4")
            
            tmp <- data.frame(i, j, x, stringsAsFactors = T)
            
            rmat           <- sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
            rownames(rmat) <- levels(tmp$i)
            colnames(rmat) <- levels(tmp$j)
            #print("5")
            
            Rmat           <- new('realRatingMatrix', data = rmat)
            
            rec_UBCF = Recommender(Rmat, method = 'UBCF',
                                   parameter = list(normalize = 'Z-score', 
                                                    method = 'Cosine', 
                                                    nn = 25))
            #print("6")
            recom = predict(rec_UBCF, 
                            Rmat[1], type = 'ratings')
            
            #print("7")
            mymatrix <- as(recom, 'matrix')
            mymatrix[is.na(mymatrix)] <- 0
            mymatrix <- as.data.frame(t(mymatrix))
            mymatrix <- mymatrix %>% arrange(desc(u0))
            
            the_movies <- gsub("m","",rownames(mymatrix))[1:10]
            
            #print("8")
            return(the_movies)

            
        }) # still busy
        
    }) # clicked on button    
    
    # display the recommendations
    output$results <- renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df()
        
        #print("9")
        
    
        
        #recom_result <- movies[1:10, , drop = TRUE]$MovieID
        
        #print(recom_result)
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[movies$MovieID == recom_result[(i - 1) * num_movies + j]][1], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[movies$MovieID == recom_result[(i - 1) * num_movies + j]][1])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
}) # server function