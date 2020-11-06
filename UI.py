from time import sleep
import pygame, sys
from pyswip import Prolog, Atom, Functor

prolog = Prolog()
prolog.consult("planning")

pygame.init()

WIDTH  = 800  
HEIGHT = 600

DELAY_TIME = 2

VIDEO_POSITION = ( 300, 212)
VIDEO_SIZE = ( 300, 200)

screen = pygame.display.set_mode(( WIDTH, HEIGHT))
clock  = pygame.time.Clock()

bg_surface = pygame.image.load('./image/background.PNG')
bg_surface = pygame.transform.scale(bg_surface, (WIDTH, HEIGHT))

playerImg = pygame.image.load('./image/charlecter.png')
playerImg = pygame.transform.scale( playerImg, ( 50, 50))

taskImg = pygame.image.load('./image/task.png')
taskImg = pygame.transform.scale(taskImg, ( 50, 50))

def loadImage( inputList, pathLocation, end ):
    print('loading images...')

    for item in range( 1, end):
        path = ( pathLocation + f' ({item}).tif')
        image = pygame.image.load( path )
        image = pygame.transform.scale( image , VIDEO_SIZE )
        inputList.append( image )

cleanO2Imgs = []
IMAGE_FILE_LENGTH = 136
loadImage( cleanO2Imgs, './video/cleanO2filter/cleanFilter', IMAGE_FILE_LENGTH)

fixingLightImgs = []
IMAGE_FILE_LENGTH = 65
loadImage( fixingLightImgs, './video/fixingLight/fixLight', IMAGE_FILE_LENGTH)

navigationImgs = []
IMAGE_FILE_LENGTH = 118
loadImage( navigationImgs, './video/navigation/navigation', IMAGE_FILE_LENGTH)

reactorImages = []
IMAGE_FILE_LENGTH = 429
loadImage( reactorImages, './video/reactor/reactor', IMAGE_FILE_LENGTH)

rebootWifiImages = []
IMAGE_FILE_LENGTH = 235
loadImage( rebootWifiImages, './video/rebootWifi/rebootWifi', IMAGE_FILE_LENGTH)

spots = {'cafereria':(400, 50) ,
         'northeasthallway':(510 , 110),
         'weapons':( 565, 57),
         'o2': ( 520, 190),
         'navigation':(690, 210),
         'easthallway':(590, 280),
         'shields':(550, 380),
         'communications': (480, 450),
         'southeasthallway':(450, 400),
         'storage':(380, 365),
         'centerhallway':(400, 310),
         'admin':( 490, 280),
         'electrical':( 270, 330),
         'southwesthallway':(250, 460),
         'lowerengine':(110, 350),
         'westhallway':(120, 260),
         'reactor':(57, 220),
         'security':( 180, 225),
         'upperengine':(115, 85),
         'northwesthallway':(250, 120),
         'medbay':(260, 170)}

taskVideos = {
    spots['o2'] : cleanO2Imgs,
    spots['electrical'] : fixingLightImgs,
    spots['navigation'] : navigationImgs,
    spots['reactor'] : reactorImages,
    spots['communications'] : rebootWifiImages
}

queryTasks = ['goal(clean(filter,true))',
         'goal(chart(course,true))',
         'goal(fix(wiring,true))',
         'goal(start(reactor,true))',
         'goal(reboot(wifi,true))']

missionRooms = { 'goal(clean(filter,true))':'o2',
                 'goal(chart(course,true))':'navigation',
                 'goal(fix(wiring,true))':'electrical',
                 'goal(start(reactor,true))':'reactor',
                 'goal(reboot(wifi,true))':'communications'}

taks = []

taksSpikes = []

class Player( pygame.sprite.Sprite ):
    def __init__( self , image = None):
        pygame.sprite.Sprite.__init__( self )

        self.width = 50
        self.height = 50

        if image == None:
            self.image = pygame.Surface(  ( self.width, self.height ))
            self.image.fill( (0, 255, 0) )
        else:
            self.image = image
            
        self.rect = self.image.get_rect()
        self.rect.center = ( spots['cafereria'])
        self.position = spots['cafereria']

    def setWidth( self , width ):
        self.width = width

    def setHeight( self , height ):
        self.height = height

    def setImage( self ):
        self.image = pygame.Surface(  ( self.width, self.height ))

    def update( self ):
        self.rect.x = self.position[0]
        self.rect.y = self.position[1]

    def setPosition( self, position ):
        self.position = position
    
    def getPosition( self ):
        return self.position

class TaskVideo():
    def __init__( self ):
        self.DELAY_TIME = 100000
        self.images = []
    
    def displayImage( self ):
        for image in self.images:
            screen.blit( image , VIDEO_POSITION ) 
            pygame.display.update()
            self.delay()

    def delay( self ):
        for _ in range( self.DELAY_TIME ):
            pass

    def setImages( self, images ):
        self.images = images 

def displayTask():
    print('0: Clean o2 filter')
    print('1: Chart course')
    print('2: Fix wiring')
    print('3: Start reactor')
    print('4: Reboot wifi')
    print('5 or more: exit')

def getMoveList( queryList ):
    movePath = []
    #for path in spots:
    #    movePath.append( spots[path] )
    for soln in prolog.query("a_star(_,P)"):
        for p in soln["P"]:
            if not isinstance(p, Functor):
                #print("task", p)
                continue
            for args in p.args:
                args = str(args)
                #print("goto", args)
                movePath.append(spots[args])
        break

    return movePath

def getPath( taks ):
    inputing = True

    print("=========input task=========") 
    while inputing:
        displayTask()
        
        userInput = int( input("\ninput place you task: ") )
        if userInput >= 5:
            inputing = False 
        else:
            taks.append( queryTasks[userInput] )
            prolog.assertz(queryTasks[userInput])

    print("=========end input task=========")

    return getMoveList( queryTasks )

def delay( delayTime ):
    for _ in range( delayTime * 10000000 ):
        pass

def inputEvent():
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

        if event.type == pygame.MOUSEBUTTONUP:
            pos = pygame.mouse.get_pos()
            print( pos )

def clearTaskSpikes( taksSpikes ):
    for taksSpike in taksSpikes:
        taksSpike.setPosition( (-50, -50) )

def displayVideo( previousePlayerPosition, taskPosition, taksSpikes, taskVideo ):
    taskIndex = taskPosition.index( previousePlayerPosition )
    taskPosition.pop( taskIndex )
    clearTaskSpikes( taksSpikes )

    for task in range( len(taskPosition) ):
        taksSpikes[ task ].setPosition( taskPosition[task] )

    taskVideo.setImages( taskVideos[ previousePlayerPosition ] )
    taskVideo.displayImage()

###########################################################################################################################################
#add player
all_sprites = pygame.sprite.Group()
player = Player( playerImg )
all_sprites.add( player )

#taskVideo
taskVideo = TaskVideo()

#add task
for i in range( 5 ):    
    task = Player( taskImg )
    taksSpikes.append( task )
    task.setPosition( (-50, -50) )
    all_sprites.add( task )
    
#init change var
moveList = []
taskPosition = []
listIndex = 0
previousePlayerPosition = 0
displayVideoFlag = False
getInput = True

while True:
    inputEvent()
    
    #input task
    if getInput == True:
        for task in taks: prolog.retract(task)
        
        taks = []

        player.setPosition( spots[ 'cafereria' ] )
        moveList = getPath( taks )
        getInput = False
        
        #setPoition for task
        for task in range( len( taks ) ):
            taskPoistion =  taks[ task ] 
            missionName = missionRooms[ taskPoistion ]

            taksSpikes[ task ].setPosition( spots[missionName] )
            taskPosition.append( spots[missionName] )

    #finsh all tasks
    elif listIndex == len(moveList):
        displayVideo( previousePlayerPosition, taskPosition, taksSpikes, taskVideo )
        
        #reset var
        spotLis = []
        listIndex = 0
        displayVideoFlag = False
        getInput = True

        #rest position 
        clearTaskSpikes( taksSpikes )
        player.setPosition( spots[ 'cafereria' ] )
        
        delay( DELAY_TIME )

    #move player
    else:
        player.setPosition( moveList[ listIndex ] )
        playerPosition = player.getPosition()

        #player at task
        if playerPosition in taskPosition:
            previousePlayerPosition = playerPosition
            displayVideoFlag = True

        elif( displayVideoFlag == True):
            displayVideo( previousePlayerPosition, taskPosition, taksSpikes, taskVideo )
            displayVideoFlag = False

        for task in range( len(taskPosition) ):
            taksSpikes[ task ].setPosition( taskPosition[task] )
        
        listIndex += 1

        delay( DELAY_TIME )

    all_sprites.update()
    
    screen.blit(bg_surface, (0,0))
    all_sprites.draw( screen )
    
    pygame.display.update()
   
    clock.tick( 1 )
