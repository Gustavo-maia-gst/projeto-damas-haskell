import CursesWrapper
import EventLoop as E
import InitialScreen


main :: IO()
main = do
    initWrapper

    choice <- options 0

    E.init choice

