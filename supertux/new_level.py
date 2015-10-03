# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2015 Karkus476 <karkus476@yahoo.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import os

from PyQt4 import QtGui
from PyQt4.Qt import QTextEdit

from .level import Level
from random import choice
from math import ceil
from flexlay import Config

class NewLevelDialog(QtGui.QWizard):
    def __init__(self, parent):
        super(NewLevelDialog, self).__init__(parent)
        
        self.setWindowTitle("New Level Wizard")
        #Modal means you cannot touch the parent until this
        # closes
        self.setModal(True)
        
        self.level = Level()
        self.level_name = "Unnamed Level"
        self.level_author = "Anonymous"
        self.level_music = ""
        self.level_background = ""
        self.level_license = "GPL 2+ / CC-by-sa 3.0"
        self.level_width = 100
        self.level_height = 50
        self.level_spawn = (5, 25)
        
        #Each function returns a QWizardPage, which is then added
        self.addPage(self.create_intro_page())
        self.addPage(self.create_main_page())
        self.addPage(self.create_license_page())
        
        #When 'Finish' pressed run finish()
        self.finished.connect(self.finish)
        #When 'Cancel' pressed run cancel()
        self.rejected.connect(self.cancel)
        
    def finish(self):
        '''Executed when "Finish" button clicked'''
        self.level = Level.from_size(self.level_width, self.level_height)
        #self.level.current_sector.music = self.level_music
        #self.level.current_sector.background = self.level_image
        self.level.name = self.level_name
        self.level.author = self.level_author
        self.level.license = self.level_license
        self.level.current_sector.music = self.level_music
        self.level_spawn = ceil(self.level_width / 10), int(self.level_height/2)
        
    #Not Implemented
    def cancel(self):
        self.level = None
    
    #<Introduction Page>
    def create_intro_page(self):
        '''Creates the intro page containing a bit of text
        @return: QWizardPage Introduction Page'''
        page = QtGui.QWizardPage()
        
        page.setTitle("Welcome to the New Level Wizard")
        click_next =  QtGui.QLabel("Click 'Next' to continue.\n"+
                                   "In the future, this page will show some "+
                                   "checkboxes to allow experienced users "+
                                   "to view extra pages in this wizard.")
        click_next.setWordWrap(True)
        click_next.setStyleSheet("QLabel {\nfont-size: 15px;\n}")
        
        vbox = QtGui.QVBoxLayout()
        vbox.addWidget(click_next)
        
        page.setLayout(vbox)
        return page
    
    #<Main Page>
    
    def create_main_page(self):
        '''Creates the general page containing inputs to
        type in level name, author and size as well as
        browse through files for music and background
        image files
        @return: QWizardPage General Information Page'''
        page = QtGui.QWizardPage()
        page.setTitle("General Information")
        
        #A list of names randomly selected as the 'Example
        # Level name.
        # Feel free to add more
        fun_names = ("Fire Meets Ice", 
                     "Flipper Hell", 
                     "Level 4", 
                     "Into the Deep", 
                     "Unknown Flying Penguin")
        
        #choice is random.choice
        self.level_name = "Example: " + choice(fun_names)
        
        name_input = QtGui.QLineEdit(self.level_name)
        #Run self.set_name whenever text changes
        name_input.textChanged[str].connect(self.set_name)
        
        author_input = QtGui.QLineEdit("Anonymous")
        #Run self.set_author whenever text changes
        author_input.textChanged[str].connect(self.set_author)
        
        w_in = QtGui.QLineEdit("100")
        #Run self.set_width whenever text changes
        w_in.textChanged[str].connect(self.set_width)
        
        h_in = QtGui.QLineEdit("50")
        #Run self.set_height whenever text changes
        h_in.textChanged[str].connect(self.set_height)
        
        self.music_input = QtGui.QLineEdit()
        #Run self.set_music whenever text changes
        self.music_input.textChanged[str].connect(self.set_music)
        browse_for_music = QtGui.QPushButton("Browse...")
        #Run self.browse_music whenever button clicked
        browse_for_music.clicked.connect(self.browse_music)
        
        self.img_input = QtGui.QLineEdit()
        #Run self.set_img whenever text changes
        self.img_input.textChanged[str].connect(self.set_img)
        browse_for_img = QtGui.QPushButton("Browse...")
        #Run self.browse_image whenever button clicked
        browse_for_img.clicked.connect(self.browse_image)

        #Add everything to Vertical layout
        grid = QtGui.QGridLayout()
        grid.addWidget(QtGui.QLabel("Level Name:"),0,0)
        grid.addWidget(name_input, 1, 0)
        
        grid.addWidget(QtGui.QLabel("Level Author:"),2,0)
        grid.addWidget(author_input,3,0)
        
        grid.addWidget(QtGui.QLabel("Level Width:"),4,0)
        grid.addWidget(w_in,5,0)
        grid.addWidget(QtGui.QLabel("Level Height:"),6,0)
        grid.addWidget(h_in,7,0)
        
        grid.addWidget(QtGui.QLabel("Level Music:"),8,0)
        grid.addWidget(self.music_input,9,0)
        grid.addWidget(browse_for_music,9,1)
        
        grid.addWidget(QtGui.QLabel("Level Background:"),10,0)
        grid.addWidget(self.img_input,11,0)
        grid.addWidget(browse_for_img,11,1)
        
        page.setLayout(grid)
        return page
    
    def set_name(self, text): #Connected to signal
        self.level_name = text if text is not "" else "No Name"
        
    def set_author(self, text): #Connected to signal
        self.level_author = text if text is not "" else "No Author"
        
    def set_music(self, text): #Connected to signal
        self.level_music = text if text is not "" else ""
        
    def set_height(self, text): #Connected to signal
        try:
            self.level_height = int(text) if text is not "" else 50
        except TypeError:
            self.level_height = 50
            
    def set_width(self, text): #Connected to signal
        try:
            self.level_width = int(text) if text is not "" else 100
        except TypeError:
            self.level_width = 100

    def browse_music(self): #Connected to signal
        full_path = QtGui.QFileDialog.getOpenFileName(None, "Open Music File",
                                                      os.path.join(Config.current.datadir, "music"))
        #If path goes to datadir,
        if full_path[:len(Config.current.datadir)] == Config.current.datadir:
            #Set path as relative
            self.music_input.setText(full_path[len(Config.current.datadir):])
        else:
            self.music_input.setText(full_path)

    def set_img(self, text): #Connected to signal
        self.level_background = text if text is not "" else ""
    
    def browse_image(self): #Connected to signal
        full_path = QtGui.QFileDialog.getOpenFileName(None, "Open Background Image",
                                                      os.path.join(Config.current.datadir, "images", "background"))
        #If path goes to datadir
        if full_path[:len(Config.current.datadir)] == Config.current.datadir:
            #Set path as relative
            self.img_input.setText(full_path[len(Config.current.datadir):])
        else:
            self.img_input.setText(full_path)
    
    #<License Page>
    def create_license_page(self):
        '''
        This function inits the license page of the new level
            wizard
        @return QWizardPage The License Page of the wizard
        '''
        page = QtGui.QWizardPage()
        
        page.setTitle("Level License")
        page.setSubTitle("You must set a license for your level, which "+
                         "defines how others may use and share your level_ "+
                         "In the spirit of 'free and open source' "+
                         "we ask that you make your level free "+
                         "(as in 'free speech' not 'free WiFi')")
        
        self.license_input = QtGui.QTextEdit("GPL 2+ / CC-by-sa 3.0")
        self.set_license()
        self.license_input.textChanged.connect(self.set_license)
                
        vbox = QtGui.QVBoxLayout()
        vbox.addWidget(self.license_input)
        
        page.setLayout(vbox)
        return page
    
    def set_license(self): #Connected to signal
        self.level_license = self.license_input.toPlainText()
