![screenshot showing env diagram](iLambda-logo.png)

iLambda is designed to aid teachers in setting up projects on iSENSE (www.isenseproject.org)--a system for collecting, visualizing, and sharing data.

##Written By:
* Kaitlyn Carcia (kcarcia) 
* Ravy Thok (rthok)

####Screenshot of the Login Page

![Login Page](iLambda-Login_Page.png)

####Screenshot of the Project Selection Page

![Selection Page](iLambda-Project_Selection_Page.png)

####Screenshot of the Project Title Page

![Project Title](iLambda-ProjectTitle.png)

####Screenshot of the Project Media Object Page

![Title Page](iLambda-Media_Page.png)

####Screenshot of Finish Page

![Finish Page](iLambda-Finish_Page.png)

##Concepts Demonstrated
* We used filter to parse data in the database.
* We used database modeled after a database in one of our homework assignments.
* We created HOPs to access data in the database.

##External Technology and Libraries
* We used calls in iSENSE's API to verify login credentials in iLambda.
* We also used iSENSE's racket-api to create a project.
* We also used Racket's built-in libraries: Web server and Net to create the webpage, submit through forms, and make get/post requests to the API.

####Kaitlyn's favorite line of code
This is my favorite line because string interpolation in Scheme is very messy, and I am so proud I was able to figure out how to actually do this. This line makes
a GET request to the iSENSE API by passing a URL with credentials we obtain from the form.
```scheme
(define url (format "http://isenseproject.org/api/v1/users/myInfo?email=~s&password=~s" (string->symbol (login-email cred)) (string->symbol (login-password cred))))
```
####Ravy's favorite line of code
This code is for our Media Object Page. It created a browse button on the webpage that allows the user to upload a file from their computer.
```
(input ([type "file"][class "filestyle"] [name "file"]))
```

#How to Download and Run
Simply download this repo as a ZIP or clone it. You must change a path in main.rkt to get the style to work on your computer.
The path you must change is below a comment that says, ";; MUST CHANGE PATH".
