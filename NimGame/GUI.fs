module GUI

open System
open System.Windows.Forms
open System.Drawing

let window =
  new Form(Text="Nim", Size=Size(525,825))

let ansBox =
  new TextBox(Location=Point(150,150),Size=Size(200,25))

let sizeBox =
  new TextBox(Location=Point(150,250),Size=Size(200,25))

let gameBox =
  new TextBox(Location=Point(200,350),Size=Size(100,25), Text="")

let x =
  new TextBox(Location=Point(150,425),Size=Size(100,50), Text="")

let SetupButton =
  new Button(Location=Point(200,275),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Setup Game")

let DownloadDataButton = 
    new Button(Location=Point(150,10),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Download Game Data")

let cancelButton =
  new Button(Location=Point(250,10),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="CANCEL")

let urlBox =
  new TextBox(Location=Point(150,70),Size=Size(200,25))

let userButton =
  new Button(Location=Point(150,375),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="User move")

let AIButton =
  new Button(Location=Point(250,375),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="Ai Move")

let toggle bs = 
    for b in [userButton;AIButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false
let enable bs = 
    for b in [userButton;AIButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- true