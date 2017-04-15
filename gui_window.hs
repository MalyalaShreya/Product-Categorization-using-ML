import Graphics.UI.Gtk

{-
  a functon which is called when button 4 or button 5 or button 6 or button 7 or button 8 is pressed....
  it pops up a new window and print the name of the button on the window . 
-}
new_window ::String -> IO()
new_window s = do
	initGUI
  	window  <- windowNew
	vbox    <- vBoxNew True 10
 
	set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerBorderWidth := 10, containerChild := vbox]

	label1  <- labelNew (Just "PRODUCT CATEGORISATION")
	label3  <- labelNew (Just s)
	label2	<- labelNew (Just "__________")
	--miscSetAlignment label1 0 0
  	boxPackStart vbox label1 PackNatural 0
	boxPackStart vbox label3 PackNatural 0
  	boxPackStart vbox label2 PackNatural 0
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI

{-
  a functon which is called when button 1 or button 2 or button 3 is pressed....
  it pops up a new window and print the name of the button on the window . 
-}

new_window2 ::String -> IO()
new_window2 s = do
	initGUI
  	window  <- windowNew
	vbox    <- vBoxNew True 10
 
	set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerBorderWidth := 10, containerChild := vbox]

	label1  <- labelNew (Just "PRODUCT CATEGORISATION")
 
{-	do { 
		if(s=="Naive Bayes") then var <- naive() else
		if(s=="Neural Networks") then var <- neural() else	
	 	 var <- knn()
-}
	label3  <- labelNew (Just s)
	label2	<- labelNew (Just "__________")

	-- packing everything into vbox 
  	boxPackStart vbox label1 PackNatural 0
	boxPackStart vbox label3 PackNatural 0
  	boxPackStart vbox label2 PackNatural 0
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI

{- 
  a main function to create button and defining the action on clicking those button
-}

main :: IO ()
main = do
	initGUI
	window  <- windowNew			-- creates the home window
	hbox1   <- hBoxNew True 10
	hbox2   <- hBoxNew True 10
	vbox    <- vBoxNew True 10
 

	label1  <- labelNew (Just "PRODUCT CATEGORISATION")
	--miscSetAlignment label1 0 0
	boxPackStart vbox label1 PackNatural 0
	sep1    <- hSeparatorNew
	--boxPackStart hbox1 sep1 PackNatural 10

	-- creating buttons with their names...
  	button1 <- buttonNewWithLabel "Naive Bayes"
	button2 <- buttonNewWithLabel "Nueral Networks"
	button3 <- buttonNewWithLabel "KNN"
	button4 <- buttonNewWithLabel "Test 1"
	button5 <- buttonNewWithLabel "Test 2"
	button6 <- buttonNewWithLabel "Test 3"
	button7 <- buttonNewWithLabel "Test 4"
	button8 <- buttonNewWithLabel "Test 5"

	set window [windowTitle:= "PRODUCT CATEGORISATION",windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerBorderWidth := 10, containerChild := vbox]

	-- packing the buttons in the home window
	boxPackStart hbox1 button1 PackGrow 0
	boxPackStart hbox1 button2 PackGrow 0
	boxPackStart hbox1 button3 PackGrow 0
	boxPackStart vbox hbox1 PackGrow 0
	boxPackStart hbox2 button4 PackGrow 0
	boxPackStart hbox2 button5 PackGrow 0
	boxPackStart hbox2 button6 PackGrow 0
	boxPackStart hbox2 button7 PackGrow 0
	boxPackStart hbox2 button8 PackGrow 0
	boxPackStart vbox hbox2 PackGrow 0

	-- defining the actions on clicking the respective buttons  
	(onClicked button4 (new_window ("Test 1"))) 
	(onClicked button5 (new_window ("Test 2"))) 
	(onClicked button6 (new_window ("Test 3")))  
	(onClicked button7 (new_window ("Test 4"))) 	
	(onClicked button8 (new_window ("Test 5")))
	onClicked button1 (new_window2 ("Naive Bayes"))
	onClicked button2 (new_window2 ("Nueral Networks"))
	onClicked button3 (new_window2 ("KNN"))
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
