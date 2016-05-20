Send emails with grades from a CSV file and attachments from a folder, using a template. Adapt to your needs.

# Requirements

* [Haskell platform](https://www.haskell.org/)
    * You will need to `cabal install` several packages: `parsec`, `cassava`, `MissingH`.
* Script works under cygwin. Modifications may be necessary for other shells.
    * Requires `email`. Do `apt-cyg install email` and follow [instructions](http://jingkee.resharecle.com/?p=207) to configure `email`.
	* Look up the SMTP info from your email provider. Ex. for Princeton, see [here](https://csguide.cs.princeton.edu/email/setup/imapconfig). (Replace with smtp.princeton.edu for general Princeton email.)

# Instructions

* Write an email and save it as `email.txt`. For fields that are customized, use a keyword (ex. `NAME`). (`email.txt` gives a sample.)
* Customize `Emailer.hs` as desired.
    * It expects a CSV file with no header and with columns LAST NAME, FIRST NAME, ID, (HW GRADES), HW AVERAGE, EXAM, FINAL AVERAGE.
	* Customize it with which fields you want to replace, ex. `NAME` with name.
	* Put all the attachments with students' names in one directory.
*   Compile: `ghc Emailer`.
*   Run `./Emailer` with arguments. See `sample_script`.

    > ./Emailer "Subject" "from@domain" "cc@domain" "C:/dir/to/files/" "grades.csv" "email.txt" "@domain" "output_name"
*   Script will try to find attachment corresponding to each person. (Best to name the attachments `LASTNAME_FIRSTNAME.pdf`, for example.) For each attachment name it can't resolve, it will ask you to enter the file name.
*   Script produces one text file for each email. 
*   Run the output file (default: `email_grades`). WARNING: This sends out all emails; you may want to check the text files first!


