from time import sleep
import pygame, sys

pygame.init()

WIDTH  = 800  
HEIGHT = 600

screen = pygame.display.set_mode(( WIDTH, HEIGHT))
clock  = pygame.time.Clock()

bg_surface = pygame.image.load('./image/background.PNG')
bg_surface = pygame.transform.scale(bg_surface, (WIDTH, HEIGHT))

player = pygame.image.load('./image/charlecter.png')
player = pygame.transform.scale(player, ( 50, 50))

spots = {'cafereria':(400, 50) ,
         'northeasthallway':(510 , 110),
         'weapons':( 565, 57),
         'o2': ( 520, 190),
         'navigation':(690, 210),
         'easthallway':(590, 280),
         'shields':(550, 380),
         'communication': (480, 450),
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
         'uppderengine':(115, 85),
         'northwesthallway':(250, 120),
         'medbay':(260, 170)}

queryTasks = ['goal(clean(filter,true)).',
         'goal(chart(course,true)).',
         'goal(fix(wiring,true)).',
         'goal(start(reactor,true)).',
         'goal(reboot(wifi,true)).']

missionRooms = { 'goal(clean(filter,true)).':'o2',
                 'goal(clean(filter,true)).':'navigation',
                 'goal(fix(wiring,true)).':'electrical',
                 'goal(start(reactor,true)).':'reactor',
                 'goal(reboot(wifi,true)).':'communication'}

taks = []

class Player( pygame.sprite.Sprite):
    def __init__( self ):
        pygame.sprite.Sprite.__init__( self )
        self.image = pygame.Surface((50,50))
        self.image.fill( (0, 255, 0) )
        self.rect = self.image.get_rect()
        self.rect.center = ( spots['cafereria'])
        self.position = spots['cafereria']

    def update( self ):
        self.rect.x = self.position[0]
        self.rect.y = self.position[1]

    def setPosition( self, position ):
        self.position = position

def displayTask():
    print('0: Clean o2 filter')
    print('1: Chart course')
    print('2: Fix wiring')
    print('3: Start reactor')
    print('4: Reboot wifi')
    print('5 or more: exit')

def getMoveList( queryList ):
    movePath = []
    for path in spots:
        movePath.append( spots[path] )
        
    return movePath

def getPath():
    inputing = True
    taks = []
    print("=========input task=========") 
    while inputing:
        displayTask()
        
        userInput = int( input("input place you want to go: ") )

        if userInput >= 5:
            inputing = False 
        else:
            taks.append( queryTasks[userInput] )

    print("=========end input task=========")
    print( taks )
    
    return getMoveList( queryTasks )

def delay( delayTime ):
    for i in range( delayTime * 10000000 ):
        pass

def inputEvent():
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

        if event.type == pygame.MOUSEBUTTONUP:
            pos = pygame.mouse.get_pos()
            print( pos )

all_sprites = pygame.sprite.Group()
player1 = Player()
all_sprites.add( player1 )

moveList = []
listIndex = 0
getInput = True
while True:
    inputEvent()
    
    #input task
    if getInput == True:
        player1.setPosition( spots[ 'cafereria' ] )
        moveList = getPath()
        getInput = False
         
    #finsh all tasks
    elif listIndex == len(moveList):
        spotLis = []
        listIndex = 0
        getInput = True
        
        delay( 3 )
    #move player
    else:
        player1.setPosition( moveList[ listIndex ] )
        delay( 5 )
        
    all_sprites.update()
    
    screen.blit(bg_surface, (0,0))
    all_sprites.draw( screen )
    
    pygame.display.update()
    listIndex += 1
    clock.tick( 1 )
