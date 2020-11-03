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

spotLis = []
##for i in spots:
##    spotLis.append( spots[i] )

count = 0
getInput = True

def inputTask( spotLis ):
    for i in range( 3 ):
        userInput = input("input place you want to go: ")
        spotLis.append( spots[userInput] )
        
    return spotLis, False

def delay():
    for i in range(100000000):
        pass
    
while True:

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

        if event.type == pygame.MOUSEBUTTONUP:
            pos = pygame.mouse.get_pos()
            print( pos )
            
    screen.blit(bg_surface, (0,0))
    
    if getInput == True:
        spoList, getInput = inputTask( spotLis )
        print( spoList ) 
    
    if count == len(spotLis):
        count = 0
        spotLis = []
        getInput = True
        
        delay()
    else:
        screen.blit(player, spotLis[ count ] )
        
        delay()
        
    count += 1
    pygame.display.update()
    clock.tick( 1 )
