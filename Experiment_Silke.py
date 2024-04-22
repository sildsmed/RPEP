from psychopy import visual, core, gui, event, data
import random, os, time
import pandas as pd
import numpy as np
# Import color file and tile distributions
from colour_file import colour_map
colour_map_norm = np.array(colour_map) / 255.0 # rescale from 0-255 RGB in file to 0-1 RGB in psychopy
from bandits11_l2 import bandits #np.array of distributions

## Define functions
def draw_grid(): #used for drawing the grid
    for tiles_data in tiles.values():
        tiles_data['rect'].draw()
        tiles_data['text'].draw()

def draw_grid_info(block_total, block_mean): #used for giving either the total or mean score to the participant and number of clicks left
    # different info for different kind of block
    if trial['block type'] == 'mean':
        trial_info.text = "Je gemiddeld aantal punten is " + str(block_mean) + "."
        trial_info.draw()
    else:
        trial_info.text = "Je totaal aantal punten is " + str(block_total) + "."
        trial_info.draw()

def message(message_text = "", response_key = "space", duration = 0, height = 0.05, pos = (0.0, 0.0), color = "white"): # make a function for presenting messages on screen
    message_on_screen = visual.TextStim(win, text = "OK")
    message_on_screen.text    = message_text
    message_on_screen.height  = height
    message_on_screen.pos     = pos
    message_on_screen.color   = color
    
    message_on_screen.draw()
    win.flip()
    event.waitKeys(keyList = response_key)

## File management and dlg box
# Participant information dictionary
info = {"Participant Nummer": "", "Leeftijd": "", "Gender": ["mannelijk", "vrouwelijk", "x"], "schrijfhand": ["links", "rechts", "beide"]}
dlg = gui.DlgFromDict(dictionary=info, title='Informatie deelnemer')

#set current working directory and add an extra folder for data
my_directory= os.getcwd() + '/data_SilkeDeSmedt' 
#create the data folder if it doesn't exist already
if not os.path.isdir(my_directory):
    os.mkdir(my_directory)

participant_number = info['Participant Nummer']
file_name= my_directory + "/participant"+ str(info['Participant Nummer'])

# initialize the experiment handler
this_exp = data.ExperimentHandler(dataFileName = file_name, extraInfo = info)

## Experiment parameters
n_trials = 3
n_blocks = 3
n_blocks += 0 # Add one for practice grid
tot_n_trials = n_trials * n_blocks

# block type order is defined by participant number
block_types = ['total']
for block_i in range(1, n_blocks):
    if int(participant_number) %2 != 0:
        block_types.append('total' if block_i %2 != 0 else 'mean')
    else:
        block_types.append('mean' if block_i %2 != 0 else 'total')

# make an empty trial matrix
design = ["block number", "block type", "trial number", "practice"]
trials = np.empty((tot_n_trials, len(design)), dtype=object)

# Fill in block number, block type, trial number within block, and practice flag for each trial
for i in range(tot_n_trials):
    block_number = i // n_trials + 1  # +1 because block numbers start from 1
    block_type = block_types[block_number - 1]  # -1 because list indices start from 0
    
    # Calculate trial number within block
    trial_number_within_block = i % n_trials   #because trial numbers start from 1
    
    trials[i, 0] = block_number
    trials[i, 1] = block_type
    trials[i, 2] = trial_number_within_block
    trials[i, 3] = 1 if block_number == 1 else 0  # Set practice flag to 1 for the first block, 0 otherwise

print(trials)
# Convert the trial matrix to a pandas DataFrame
trials_df = pd.DataFrame(trials, columns=["block number", "block type", "trial number", "practice"])

# convert the dataframe to a list of dictionaries
trial_list = pd.DataFrame.to_dict(trials_df, orient="records")  # trial_list is list of dicts
print("**")
print(trial_list)

## Grid initialisation
# Empty dictionary for the tiles 
tiles = {}

# Define grid parameters
grid_size = 11
square_size = 0.1 
grid_spacing = square_size + 0.01 # size of the square + spacing

# Calculate grid starting position in norm units
start_x = -((grid_size - 1) * grid_spacing) / 2 # /2 because norm system middle at 0.0, grid size * spacing to the left 
# (-1 so it starts at the middle of the first tile and doesn't overshoot)
start_y = ((grid_size - 1) * grid_spacing) / 2

## Window, text and clock initialisation
# Set up the window
win = visual.Window(size = (800, 800), units='norm')

# initialize text for the participant
instructions = ("Welkom bij het keuze experiment. \n\n Tijdens dit experiment is het jouw doel om zoveel mogelijk punten te verzamelen. Je zal een raster te zien krijgen met enkele onthulde " +
                "en andere verborgen vakjes. Je kan op elk vakje klikken, waardoor je de punten krijgt die bij dit vakje horen. Als je op een verborgen vakje "+
                "klikt onthul je dit vakje en krijg je de achterliggende punten. Probeer zoveel mogelijk punten te verzamelen door te klikken op de vakjes." +
                "\n\n Op het volgende scherm wordt je een voorbeeldraster getoond." +
                "\n\n Druk op de spatiebalk om verder te gaan.")
example = ("Dit is een voorbeeld van hoe de punten verdeeld kunnen zijn. Zoals je ziet, zijn vakjes die dicht bij elkaar liggen, gelijkend" +
            " in het aantal punten dat ze bezitten. \n\n Druk op de spatiebalk om verder te gaan")
practice2 = "Tijd om te oefenen! Probeer zoveel mogelijk punten te verzamelen. \n\n Druk op de spatiebalk om verder te gaan."
practice_complete = ("De oefenfase is nu voltooid! \n\n Als je het nog niet helemaal snapt, kan je aan de proefleider vragen voor meer uitleg." +
                    "\nHet echte experiment zal nu beginnen.\n\n Druk op de spatiebalk om verder te gaan.")
goodbye           = ("Dit is het einde van het experiment.\n\n"+
                    "Dankje voor je deelname!\n\n"+
                    "Druk op de spatiebalk om verder te gaan")
block_end = "Druk op de spatiebalk om verder te gaan naar de volgende ronde."

# initialize the trial info the participant sees underneath the grid
trial_clicks_left = visual.TextStim(win, pos = (0, -0.73), text = "", height = 0.05)
trial_info = visual.TextStim(win, pos = (0, -0.67), text = "", height = 0.05)

# initialize mouse
mouse = event.Mouse(win)

# initialize clock
my_clock        = core.Clock()

# show the instructions
message(message_text = instructions)

# show the example grid
practice_grid_image = visual.ImageStim(win, image = "practice_grid.png", pos = (0, 0.4), size = 0.7, interpolate=True)
practice_grid_image.draw()
message(message_text = example, pos = (0, -0.25))

## THE EXPERIMENT
# initialize trial handler
trials = data.TrialHandler(trialList = trial_list, nReps = 1, method = "sequential") 
this_exp.addLoop(trials)

previous_block_number = 0

for trial in trials: 
    if previous_block_number == 0 or previous_block_number != trial['block number']:
        if trial['block number'] == 1:
            # Show practice2 message before the first block
            message(message_text=practice2)

        reward = bandits[np.random.randint(len(bandits))]  # Define the parameters of the grid
        
        for row in range(grid_size):
            for col in range(grid_size):
                index = row * grid_size + col  # for defining which tile it is
                # Position of the tile:
                x_pos = start_x + col * grid_spacing
                y_pos = start_y - row * grid_spacing
                tile_pos = (x_pos, y_pos)
                # tile name
                tile_name = f"tile_{index + 1}"
                # tile number
                tile_reward = round(reward[index])
                # tile color based on colour_map
                tile_color = tuple(colour_map_norm[tile_reward])
                # make the tiles and text stims
                tile_rect = visual.Rect(win, width=square_size, height=square_size, pos=(x_pos, y_pos), fillColor= 'saddlebrown', lineColor = 'black', interpolate=True)
                text_stim = visual.TextStim(win, text= '', pos=(x_pos, y_pos), height=0.05, color='black')

                # Add the tile rect and text stim to the dictionary
                tiles[tile_name] = {'rect': tile_rect, 'text': text_stim, 'color': tile_color, 'reward': tile_reward, 'name': tile_name, 'posx': x_pos, 'posy': y_pos}
        
        # choose 1 random tiles that is revealed
        revealed_tile = random.choice(list(tiles.values()))
        revealed_tile['text'].text = revealed_tile['reward']
        revealed_tile['rect'].fillColor = revealed_tile['color']
        
        # make a list where the clicked tiles for every block are stored
        clicked_tiles = [revealed_tile]
        clicked_rewards = []
        consecutive_distance = []

        # initialize block total and block mean
        block_total = 0
        block_mean = 0
    
    # draw the grid: tiles & text and information
    draw_grid()
    draw_grid_info(block_total, block_mean)
    trial_clicks_left.text = "Je kan nog " + str(n_trials - trial['trial number']) + " keer klikken."
    trial_clicks_left.draw()
    win.flip()
    
    # mouse click initialisation
    mouse.clickReset()
    my_clock.reset()
    
    clicked = False  # becomes true if a cell is clicked
    # a trial only ends when one of the cells is clicked
    # loop until detect a click inside one of the cells:
    while not clicked:
        # check if anything is clicked
        for tile_name, tiles_data in tiles.items():
            if mouse.isPressedIn(tiles_data['rect']):
                clicked_tiles.append(tiles_data) #save clicked tile in list
                
                # record the RT
                RT = my_clock.getTime()
                trials.addData("RT", RT)

                # if a tile is reclicked, add noise
                if tile_name in [tile['name'] for tile in clicked_tiles]:
                    noise = int(round(np.random.normal(0, 1))) 
                    tiles_data['reward'] += noise

                clicked = True  # save that something is clicked, so that they stop listening to mouse clicks, and the trial ends

                # change the clicked tile to reveal the reward and color
                tiles_data['text'].text = int(tiles_data['reward'])
                tiles_data['rect'].fillColor = tiles_data['color']
                
                clicked_rewards.append(tiles_data['reward'])
                trials.addData("clicked reward", clicked_rewards[-1]) #add reward to trials data
                
                # calculate the distance between this click and the last one
                ## Consecutive distance
                # Calculate distance between this and previous click
                prev_tile_data = clicked_tiles[-2]
                curr_tile_data = clicked_tiles[-1]
                prev_tile_posx, prev_tile_posy = prev_tile_data['posx'], prev_tile_data['posy']
                curr_tile_posx, curr_tile_posy = curr_tile_data['posx'], curr_tile_data['posy']
                
                # Calculate Euclidean distance
                distance_between_tiles = np.sqrt((curr_tile_posx - prev_tile_posx)**2 + (curr_tile_posy - prev_tile_posy)**2)
                trials.addData("consecutive distance", distance_between_tiles)
                
                ## Distance top 10
                clickedwithoutrecent = clicked_tiles[:-1]
                clicked_tiles_sorted = sorted(clickedwithoutrecent, key=lambda x: x['reward'], reverse=True)
                # Calculate the number of tiles representing the top 10%
                num_top_10_percent = max(1, int(np.ceil(len(clicked_tiles) * 0.1)))

                # Select the top 10% of tiles
                top_10_percent_tiles = clicked_tiles_sorted[:num_top_10_percent]
                
                distances_and_tiles = []
                for top_tile_data in top_10_percent_tiles:
                    top_tile_positionx, top_tile_positiony = top_tile_data['posx'], top_tile_data['posy']
                    topdistance = np.sqrt((curr_tile_posx - top_tile_positionx)**2 + (curr_tile_posy - top_tile_positiony)**2)
                    distances_and_tiles.append((topdistance, top_tile_data))

                # Sort distances and tile data pairs based on distance
                distances_and_tiles_sorted = sorted(distances_and_tiles, key=lambda x: x[0])
                closest_tile_distance, closest_tile_data = distances_and_tiles_sorted[0]
                

                # Add closest tile distance to trial data
                if trial['trial number'] == 0:
                    data_to_add = distance_between_tiles
                else: 
                    data_to_add = closest_tile_distance
                trials.addData("distance to top 10%", data_to_add)
                
                # update the mean and total
                block_total = np.sum(clicked_rewards)
                block_mean = int(block_total / (trial['trial number'] + 1))
                trials.addData('total', block_total)
                trials.addData('mean', block_mean)

                break  # to exit the for loop that keeps on listening, such that also the while loop is ended
    
    # if you are on the last click, just show the grid until the participant presses the space bar
    if trial['trial number'] == n_trials - 1:
        core.wait(0.3)
        draw_grid()
        draw_grid_info(block_total, block_mean)
        trial_clicks_left.text = 'Je kan deze ronde niet meer klikken'
        trial_clicks_left.draw()
        message(message_text = block_end, pos = (0, -0.88))

        # If this is the last trial of the first block, show practice_complete message
        if trial['block number'] == 1:
            message(message_text=practice_complete)
    
    # let the experiment handler go to the next entry
    this_exp.nextEntry()

    core.wait(0.3)
     
    previous_block_number = trial['block number']

# goodbye message
message(message_text = goodbye)
