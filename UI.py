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

spots = {'Cafereria':(400, 50) ,
         'Weapons':( 565, 57),
         'O2': ( 520, 190),
         'Navigation':(690, 210),
         'Shields':(550, 380),
         'Communication': (480, 450),
         'Storage':(380, 365),         
         'Admin':( 490, 280),
         'Electrical':( 270, 330),
         'LowerEngine':(110, 350),
         'Reactor':(57, 220),
         'Security':( 185, 225),
         'UppderEngine':(115, 85),
         'Medbay':(260, 170)}

spotLis = []
for i in spots:
    spotLis.append( spots[i] )

count = 0
while True:

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            pygame.quit()
            sys.exit()

        if event.type == pygame.MOUSEBUTTONUP:
            pos = pygame.mouse.get_pos()
            print( pos )

    if count == len(spotLis):
        count = 0
        
    screen.blit(bg_surface, (0,0))
    
    screen.blit(player, spotLis[ count ] )

    count += 1
    pygame.display.update()
    clock.tick( 1 )
