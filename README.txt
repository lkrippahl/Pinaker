************************************************************
******************                       *******************
***************            Pinaker           ***************
******************                       *******************
************************************************************
Version: 0.1 beta

************************************************************
Disclaimer:

This software is provided as is and you assume all
responsibility for its use. For all intents and purposes, 
I just put a bunch of bytes in some server.
So I'm not responsible if anything goes wrong, and will 
actually be surprised if everything works fine.

Despite this, thanks for using Pinaker :)
************************************************************

Acknowledgements:

Thanks to Lukas Gebauer for the Ararat Synapse source code,
included in the synapse folder (of the source code).

Thanks to Johann C. Rocholl for the descriptions of barcode
identification algorithms on his dissertation on 
"Robust 1D Barcode Recognition on Mobile Devices".


Quickstart:

0- You need an access key to ISBNdb (http://isbndb.com/). If
   you don't have one, create a free account with ISBNdb to
   obtain an access key.

1- Photograph your book barcodes. The images should have 
   around 3M pixels (2048x1536), with the barcode centered
   and occupying one third of the width of the image.
   Note: barcode identification is still very picky. Take care
   to focus properly and that the bars are vertical in the.
   Or you can wait for a better version of this app... 

2- Copy the .jpg files to a folder, copy the folder path
   to the image folder text box.

3- Click the "Process images" button to find the ISBN numbers.

4- Incorrect numbers are shown on the top listbox. Select the
   first one, type the correct EAN-13 number in the text box and
   press enter. The EAN-13 number is the set of 13 digits under
   the bars.

5- To retrieve book data from ISBNdb, copy your access key to
   the acces key text box and press the retrieve button. Note
   that access to ISBNdb records is limited by the server, so
   don't process too many at one time.
   This will download an .xml file for each EAN-13 number into
   the folder given for the images. Existing XML files will not
   be requested from the server unless the option to skip them
   is unchecked.

6- The "Compile CSV" button builds a tab-separated table from
   all the xml files, with title, long title, authors and
   publisher information.


Hints:

You can put a pinakerdata.txt file in the configuration folder
to automatically fill in folder and access key data when you 
run Pinaker.
The first line must be the folder path and the second line the
access key (with no trailing spaces or other characters).

In Windows the configuration folder is the folder where 
the executable was placed. In Linux it is the .config folder
in your home folder.

In Linux the file names are case-sensitive. If the .jpg 
extension for your image files are in lowercase then uncheck
the ".JPG (uppercase)" checkbox. For Windows users this does
not matter.



The source code can be found here:
https://github.com/lkrippahl/Pinaker

And executable files on my software page:
http://centria.fct.unl.pt/~ludi/software.html



************************************************************
Ludwig Krippahl, 2011. 
No copy rights reserved. My source code in this
application is dedicated to the public domain.

For licensing and copyrights on the Synapse library,
check the source files in the synapse folder.