// Code from Hansen and Rischel: Functional Programming using F#     1       6/12 2012
// Chapter 13: Asynchronous and parallel computations.          Revised MRH 25/11 2013 
// Code from Section 13.5: 13.5 Reactive programs.

#load "NimAi.fs"
#load "GUI.fs"
#load "Game.fs"

open GUI
open Game

// Initialization
window.Controls.Add ansBox
window.Controls.Add sizeBox
window.Controls.Add gameBox
window.Controls.Add x
window.Controls.Add urlBox
window.Controls.Add SetupButton
window.Controls.Add userButton
window.Controls.Add AIButton
window.Controls.Add DownloadDataButton
window.Controls.Add cancelButton
SetupButton.Click.Add (fun _ -> ev.Post (Board sizeBox.Text))
userButton.Click.Add (fun _ -> ev.Post (User x.Text))
AIButton.Click.Add (fun _ -> ev.Post (AI gameBox.Text))
DownloadDataButton.Click.Add (fun _ -> ev.Post (Download urlBox.Text))
cancelButton.Click.Add (fun _ -> ev.Post Cancel)

// Start
Async.StartImmediate (ready())
window.Show()