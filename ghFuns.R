# Read a single page
read_page <- function(pageurl) {
  doc = getForm( # Request Form
    pageurl,
    .opts = list(
      followlocation = TRUE,
      verbose = FALSE,
      cookie = readLines("Cookies.txt"), # Cookie.txt
      httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7", "User-Agent" = useragent)
    ), binary = TRUE # Turns data into binary
  )
  doc = htmlParse(rawToChar(doc)) # First raw, then Parse. Deals with unicode errors
 
  return(doc) # Return parsed HTML url
}


# Get the links to all the questions on a page
page_links = function(pageurl) {
  doc = read_page(pageurl) 
  
  # Get all the 50 questions on the page
  links = getNodeSet(doc, "//a[@class='s-link']/@href") # Href -> reference (link)
  questions = paste0("https://stackoverflow.com", links) # Base + links
  return(questions)
}


# Find next URL
nextURL = function(url) {
  doc = read_page(url)
  
  getNextURL = function(doc) {
    nxt = getNodeSet(doc, "//a[@rel = 'next']") # Gets the node containing rel = next
    if (length(nxt) == 0) # Checks if node 'next' exists
      return(character())
    getRelativeURL(xmlGetAttr(nxt[[1]], "href"), url) # Relative to input url, retrieve 'next' page
  }
  
  next_url = getNextURL(doc)
  return(next_url)
}


# Read a question page
read_question_pg = function(link){
  questiondoc = read_page(link) # Input a question link
  
  question = list(xmlValue(getNodeSet(questiondoc, # Puts all values into a list, given the path for finding question header (NOT TEXT)
                        "//div[@id = 'question-header']/h1/a/text()")))
  answers = list(xmlValue(getNodeSet(
    questiondoc,
    "//div[@id = 'answers']//div[@class = 's-prose js-post-body' and @itemprop = 'text']"), trim = TRUE))
  
  comments = list(xmlValue(getNodeSet(
    questiondoc,
    "//div[@id = 'content']//div[@class = 'post-layout--right js-post-comments-component']//ul[@class = 'comments-list js-comments-list']//li[@class = 'comment js-comment ']//div[@class = 'comment-text  js-comment-text-and-form']//div[@class = 'comment-body js-comment-edit-hide']//span[@class = 'comment-copy']"), trim = TRUE)
  )

  list_info = list(question, answers, comments)
  return(list_info)
}


# All Parsed HTMLs for Questions
parsed_questions <- function(question_page) { # Question_page meaning all the question urls, get from page_links
  list = c()
  for (i in 1:length(question_page)) { # Get HTTP for every question url
    q = getForm(
      question_page[i],
      .opts = list(
        followlocation = TRUE,
        verbose = FALSE,
        cookie = readLines("Cookies.txt"),
        httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7", "User-Agent" = useragent)
      ), binary = TRUE
    )
    if (length(q) > 0){ # If page containing information
      q = htmlParse(rawToChar(q))
    } else {
      q = character()
    }
    Sys.sleep(1) # Slow down requests to ensure no ban
    list[[i]] = q # Put parsed HTML question url into a list
  }
  return(list)
}


# All Parsed HTMLs for Comments
parsed_comments <- function(question_page) {
  result_list <- vector("list", length(question_page))
  
  for (i in 1:length(question_page)) {
    inner_links <- question_page[[i]]  # Get the inner links for the current question page
    
    if (length(inner_links) == 0) {
      result_list[[i]] <- character(0)  # Return empty vector if no inner links present
    } else {
      parsed_inner_list <- vector("list", length(inner_links))
      
      for (j in 1:length(inner_links)) {
        q <- getForm(
          inner_links[j],
          .opts = list(
            followlocation = TRUE,
            verbose = FALSE,
            cookie = readLines("Cookies.txt"),
            httpheader = c(Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7", "User-Agent" = useragent)
          ), binary = TRUE
        )
        
        if (length(q) > 0) {
          q <- htmlParse(rawToChar(q))
        } else {
          q <- character()
        }
        
        Sys.sleep(1)
        parsed_inner_list[[j]] <- q
      }
      
      result_list[[i]] <- parsed_inner_list
    }
  }
  
  return(result_list)
}



# Dataframe for Questions
questions <- function(pageurl) {
  doc = read_page(pageurl)
  
  # Questions
  questions = page_links(pageurl)
  
  # Gets all parsed HTML for question links
  parsed_question_pg = parsed_questions(questions)
  
  # Views
  views = c()
  
  for (i in 1:length(questions)){
    views[[i]] = gsub("^Viewed\r\n\\s+","",xpathSApply(parsed_question_pg[[i]], "//div[@class = 'flex--item ws-nowrap mb8 mr16']", xmlValue, trim = TRUE))
  }
  
  views = data.frame(as.character(views))
  
  # Votes
  votes = question_info(questions,
                        parsed_question_pg,
                        "//div[@class = 'question js-question']//div[@itemprop = 'upvoteCount']")
  
  # Question Text
  q_text = c()
  
  for (i in 1:length(questions)){
    q_text[[i]] = gsub("^\r\n\\s+","",xpathSApply(parsed_question_pg[[i]], "//div[@class = 'question js-question']//div[@class = 's-prose js-post-body' and @itemprop = 'text']", xmlValue, trim = TRUE))
  }
  
  q_text = data.frame(as.character(q_text))
  
  # Question Tags
  tags = question_info(
    questions, 
    parsed_question_pg,
    "//div[@class = 'question js-question']//ul[@class = 'ml0 list-ls-none js-post-tag-list-wrapper d-inline']//li[@class = 'd-inline mr4 js-post-tag-list-item']"
  )
  
  # Question Post Date
  question_posted = question_info(questions,
                                  parsed_question_pg,
                                  "//div[@class = 'question js-question']//div[@class = 'post-signature owner flex--item']//div[@class = 'user-action-time']//span[@class = 'relativetime']")
  
  # Display Name
  q_user = question_info(questions, parsed_question_pg,
                         "//div[@class = 'question js-question']//div[@class = 'post-signature owner flex--item']//div[@itemprop = 'author']//span[@itemprop = 'name']")
  
  # Reputation
  q_reputation = question_info(questions, 
                               parsed_question_pg,
                               "//div[@class = 'question js-question']//div[@class = 'post-signature owner flex--item']//div[@class = '-flair']//span[@class = 'reputation-score']")
   
      "//li[@class = 's-user-card--rep']//span[@class]/text()"
 
  
  # Question Badges
  q_badges = question_info(
    questions,
    parsed_question_pg,
    "//div[@class = 'question js-question']//div[@class = 'post-signature owner flex--item']//div[@class = 'user-details']//span[@class = 'v-visible-sr']"
  )
  
  # Question Editors and Time they edited
  q_edited_user = c()
  q_edited_time = c()
  
  for (i in 1:length(questions)) {
    edited_user = xpathSApply(
      parsed_question_pg[[i]],
      "//div[@class = 'post-signature flex--item']//div[@class = 'user-details']/a",
      xmlValue
    )
    if (length(edited_user) > 0) {
      q_edited_user[[i]] <- edited_user
    } else {
      q_edited_user[[i]] <- ""
    }
    
    edited_time = xpathSApply(
      parsed_question_pg[[i]],
      "//div[@class = 'post-signature flex--item']//div[@class = 'user-action-time' and contains(.,'edited')]//span[@class = 'relativetime']/@title"
    )
    if (length(edited_time) > 0) {
      q_edited_time[[i]] <- edited_time
    } else {
      q_edited_time[[i]] <- ""
    }
  }
  
  q_edited_user = as.data.frame(as.character(q_edited_user))
  q_edited_time = as.data.frame(as.POSIXct(as.character(q_edited_time), format = "%Y-%m-%d %H:%M:%S"))
  
  # Get combined dataframe for Questions
  combined = cbind(
    "Questions" = questions,
    "Views" = views,
    "Votes" = votes,
    "Question Text" = q_text,
    "Tags" = tags,
    "Question Date Posted" = question_posted,
    "Question Displayname" = q_user,
    "Question User Reputation" = q_reputation,
    "Badges for Question User" = q_badges,
    "Question Editors" = q_edited_user,
    "Edited time" = q_edited_time
  )
  
  colnames(combined) = c(
    "Questions",
    "Question Views",
    "Question Votes",
    "Question Text",
    "Question Tags",
    "Question Date Posted",
    "Question Displayname",
    "Question User Reputation",
    "Question User Badges",
    "Question Editors",
    "Edited Question Time"
  )
  
  return(combined)
}


# Answer function (List of Lists)
answers <- function(pageurl) {
  doc = read_page(pageurl)
  
  # Answers
  answers = page_links(pageurl)
  
  # Parsed HTML files
  parsed_answer_pg = parsed_questions(answers)
  
  # Text
  answers_text = answer_info(answers, parsed_answer_pg, "//div[@id = 'answers']//div[@class = 's-prose js-post-body' and @itemprop = 'text']")
  
  # Answerer
  user_answer = answer_info(answers, parsed_answer_pg, "//div[@id = 'answers']//div[@class = 'post-signature flex--item fl0' or @class = 'post-signature owner flex--item fl0']//div[@class = 'user-details' and @itemprop = 'author']//span[@itemprop = 'name']")

  # Answer Post Date
  post_answer = answer_info(answers, parsed_answer_pg, "//div[@id = 'answers']//div[@class = 'post-signature owner flex--item fl0' or @class = 'post-signature flex--item fl0']//div[@class = 'user-action-time']//span[@class = 'relativetime']")

  # Answer Reputation
  answers_rep = answer_info(answers, parsed_answer_pg, "//div[@id = 'answers']//div[@class = 'post-signature flex--item fl0' or @class = 'post-signature owner flex--item fl0']//div[@class = 'user-details' and @itemprop = 'author']//span[@class = 'reputation-score']/text()")
  
  # Answer Badges
  answers_badges = answer_info(answers, parsed_answer_pg, "//div[@id = 'answers']//div[@class = 'post-signature flex--item fl0' or @class = 'post-signature owner flex--item fl0']//div[@class = 'user-details' and @itemprop = 'author']//div[@class = '-flair']//span[@class = 'v-visible-sr']")
  
  # Combined Answers
  combined = list(answers_text,
                   user_answer,
                   post_answer,
                   answers_rep,
                   answers_badges)

  names(combined) = c("Ans Body", "Ans User","Ans Post Date", "Ans Reputation", "Ans Badges")
  
  return(combined)
}


# New Comments Function
comments <- function(pageurl){

  comments = page_links(pageurl)
  
  # Get parsed HTML pages for questions
  parsed_comment_pg = parsed_questions(comments)
  
  # Get the node for comment
  comment_node = list()
  # Get node for Comment ID
  comment_url_node = list()
  
  # Link to show xyz more comments
  linktoshow = "js-show-link comments-link "
  # Comment ID of answer
  link_id = list()
  # Comment URLs
  commenturls = list()
  
  
  for (i in 1:length(parsed_comment_pg)){ # For every parsed HTML comment pg
    if(length(getNodeSet(parsed_comment_pg[[i]], "//div[@id = 'answers']//div[@class = 'post-layout--right js-post-comments-component']/div[2]/a[2]")) > 0){ # If there is a separate comment page
      comment_node[[i]] = getNodeSet(parsed_comment_pg[[i]], "//div[@id = 'answers']//div[@class = 'post-layout--right js-post-comments-component']/div[2]/a[2]") # This specific path gets all the comment pages for each question/answer on ONE POST, even if no comments exist at all
      comment_url_node[[i]] = getNodeSet(parsed_comment_pg[[i]], "//div[@id = 'answers']//div[@class = 'post-layout--right js-post-comments-component']/div[2]") # This gets the unique ID for each comment page
    } else{
      comment_node[[i]] = character()
      comment_url_node[[i]] = character()
    }
    
    
  }

  for (j in 1:length(comments)){
    comment_list = list() # Empty list for each iteration, we will put the value of this into another list for each iteration of i
    if (!length(comment_url_node[[j]]) == 0){ # Check if there is a comment 
      for (k in 1:length(comment_url_node[[j]])) { # For the list of comment nodes inside a list j, in comment_url_node
        link_id[[k]] = (gsub("\\D","",(xmlGetAttr(comment_url_node[[j]][[k]],"id")))) # This extracts the unique Comment ID for each comment node there is for a question/answer
        comment_list[[k]] =  paste0("https://stackoverflow.com/posts/", link_id[[k]], "/comments?") # This gets the page containing all the comments for one question/answer, and puts it in a list
      } 
      commenturls[[j]] = comment_list
      } else {
      commenturls[[j]] = character()
      }
    } 
    
  
  parsed_comments_page = parsed_comments(commenturls)
  
  # browser()
  
  comment_text = comment_info(commenturls, parsed_comments_page, "//li[@class = 'comment js-comment ']//div[@class = 'comment-text  js-comment-text-and-form']//div[@class = 'comment-body js-comment-edit-hide']//span[@class = 'comment-copy']")
  
  comment_user = comment_info(commenturls, parsed_comments_page, "//li[@class = 'comment js-comment ']//div[@class = 'comment-text  js-comment-text-and-form']//div[@class = 'comment-body js-comment-edit-hide']//div[@class = 'd-inline-flex ai-center']//a/text()")
  
  comment_date = comment_info(commenturls, parsed_comments_page, "//li[@class = 'comment js-comment ']//div[@class = 'comment-text  js-comment-text-and-form']//div[@class = 'comment-body js-comment-edit-hide']//span[@class = 'comment-date']//span/text()")
  
  combined = list(
    comment_text,
    comment_user,
    comment_date
  )
  
  names(combined) = c("Comment Text", "Comment User", "Comment Date")
  
  return(combined)
}


# All Search Results in One DF
pages_search_result <- function(pageurl) {
  
  # Insert Q function
  question = questions(pageurl)
  # Insert A function
  answer = answers(pageurl)
  # Insert C function
  comment = comments(pageurl)
  
  answertemp = data.frame(lapply(answer, I)) # I is used to preserve list convertion into df
  commenttemp = data.frame(lapply(comment, I))
  
  combined = data.frame(question, answertemp, commenttemp)
  return(combined)
}


# Returns information for Quetions
question_info <- function(question_pages, parsed_question_pg, path) {
  any_info = list()
  for (i in 1:length(question_pages)) {
    info = xpathSApply(parsed_question_pg[[i]], path, xmlValue, trim = TRUE) # Retrieves tags, users, badges, i.e., just need to specify the path
    if (length(info) > 0) { # If there contains information (not blank)
      any_info[[i]] <- info
    } else { # If info is blank/NULL
      any_info[[i]] <- character()
    }
  }
  any_info = as.data.frame(sapply(any_info, paste, collapse = ', ')) # Separator
  return(any_info)
}


# Returns information for Answers
answer_info <- function(question_pages, parsed_question_pg, path) {
  any_info = list()
  for (i in 1:length(question_pages)) {
    info = xpathSApply(parsed_question_pg[[i]], path, xmlValue, trim = TRUE)
    if (length(info) > 0) {
      any_info[[i]] <- lapply(info, list) # Adds all the information to a list, since there may be multiple answer info for a question
    } else {
      any_info[[i]] <- character()
    }
  }
  
  return(any_info)
}


comment_info <- function(question_pages, parsed_question_pg, path) {
  any_info <- list()
  
  for (i in 1:length(parsed_question_pg)) {
    info <- list()  # Reset the 'info' list for each iteration
    
    if (class(parsed_question_pg[[i]]) == "HTMLInternalDocument") {
      # If 'parsed_question_pg[[i]]' is an HTMLInternalDocument, directly apply the XPath expression
      info <- xpathSApply(parsed_question_pg[[i]], path, xmlValue, trim = TRUE)
    } else if (is.list(parsed_question_pg[[i]])) {
      for (j in 1:length(parsed_question_pg[[i]])) {
        if (class(parsed_question_pg[[i]][[j]]) == "HTMLInternalDocument") {
          # If 'parsed_question_pg[[i]][[j]]' is an HTMLInternalDocument, apply the XPath expression
          info[[j]] <- xpathSApply(parsed_question_pg[[i]][[j]], path, xmlValue, trim = TRUE)
        }
      }
    }
    
    if (length(info) > 0) {
      any_info[[i]] <- info
    } else {
      any_info[[i]] <- character()
    }
  }
  
  return(any_info)
}


# Next Checker
# next_page = function(url){
#   prev_url = url
# 
#   while (TRUE) { # While there is @rel = 'next' in a url
#     doc = read_page(url)
#     nxt = getNodeSet(doc, "//a[@rel = 'next']")
#     prev = getNodeSet(doc, "//a[@rel = 'prev']")
# 
#     if (length(nxt) == 0) { # If no more next, return last page
#       prev_url = getRelativeURL(xmlGetAttr(prev[[1]],"href"),url)
#       return(prev_url)
#     } else {
#       url = getRelativeURL(xmlGetAttr(nxt[[1]], "href"), url)
#     }
#   }
# }

next_page = function(url){
  prev_urls = c()  # Initialize an empty vector to store previous URLs
  
  while (TRUE) {
    doc = read_page(url)
    nxt = getNodeSet(doc, "//a[@rel = 'next']")
    prev = getNodeSet(doc, "//a[@rel = 'prev']")
    
    if (length(nxt) == 0) { # If no more next, return last page
      if (length(prev) > 0) { # Check if there is a previous button
        prev_url = getRelativeURL(xmlGetAttr(prev[[1]], "href"), url)
        return(prev_url)
      } else {  # No previous button found
        return("No next or previous button found.")
      }
    } else {
      url = getRelativeURL(xmlGetAttr(nxt[[1]], "href"), url)
      prev_urls = c(prev_urls, url)  # Store the previous URL
    }
  }
}



# Find Last Page
last_page = function(url){ # Assuming the third page of processed results is the input url (since we are only processing pages 1-3, and then the last)
  doc = read_page(url)
  
  getLastURL = function(doc) { 
    nxt = getNodeSet(doc, "//div[@class = 's-pagination site1 themed pager float-left']/a[6]") # Specific path for finding largest page number on 3rd page of search query result
    if (length(nxt) == 0)
      return(character())
    getRelativeURL(xmlGetAttr(nxt[[1]], "href"), url)
  }
  
  next_url = getLastURL(doc) # This grabs the largest pg (i.e Pg. 9816 as the largest seen on page 3)
  last = next_page(next_url) # Apply page 9816 to this function, and keep grabbing next page until last
  
  return(last)
}
