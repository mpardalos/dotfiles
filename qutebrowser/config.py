from qutebrowser.config.config import ConfigContainer  # noqa: F401
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401

c : ConfigContainer
config : ConfigAPI

c.aliases = dict(
    w='session-save',
    q='tab-close',
    xa='close'
)

c.url.start_pages = ['https://startpage-45b51.firebaseapp.com/']
c.url.searchengines = dict(
    DEFAULT='https://duckduckgo.com/?q={}',
    r='reddit.com/r/{}',
    g='google.com/search?q={}',
    aw='https://wiki.archlinux.org/index.php?title=Special%3ASearch&search={}',
    tpb='https://www.thepiratebay.org/search/{}',
    anime='https://www8.9anime.is/search?keyword={}',
    y='https://www.youtube.com/results?search_query={}'
)


c.fonts.completion.entry = '11pt Noto Sans Display'
c.fonts.completion.category = '12pt Noto Sans Display' 

