#--------------------------------------------------------------------------------
import os
import requests
#--------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
# Dictionary that maps league names to their respective codes for scraping
LEAGUES = {
    "PremierLeague" : "E0",
    "Championship"  : "E1",
    "SerieA"        : "I1",
    "SerieB"        : "I2",
    "LaLiga1"       : "SP1",
    "LaLiga2"       : "SP2",
    "Ligue1"        : "F1",
    "Ligue2"        : "F2",
    "Bundesliga1"   : "D1",
    "Bundesliga2"   : "D2"
}
#--------------------------------------------------------------------------------

# Class to scrape football data from football-data.co.uk
#--------------------------------------------------------------------------------    
class LeagueScraper():
    def __init__(self, league, season):
        """
        Initialize LeagueScraper with league and season.
        """
        self.league = LEAGUES[league]
        self.season = season
        self.csv = None

    def download(self):
        """
        Download CSV file from the specified URL.
        """
        url = f"https://www.football-data.co.uk/mmz4281/{self.season}/{self.league}.csv"
        try:
            response = requests.get(url)
            response.raise_for_status()  # Raise HTTPError for bad responses
            self.csv = response.text
            print("File downloaded successfully.")
        except requests.exceptions.RequestException as e:
            print(f"Failed to download file: {e}")

    def savecsv(self, filepath):
        """
        Save downloaded CSV data to a file.
        """
        if self.csv:
            try:
                # Check if file already exists
                if os.path.exists(filepath):
                    print("File already exists.")
                    return

                with open(filepath, "w") as f:
                    f.write(self.csv)
                print(f"File saved successfully at {filepath}")
            except IOError as e:
                print(f"Error saving file: {e}")
        else:
            print("No CSV to save, download it first.")

if __name__ == "__main__":
    # year to scrape data
    season = "2324"  
    for league in LEAGUES.keys():
        filepath = f"../data/season_{season}/{league}_{season}.csv" 
        try:
            scraper = LeagueScraper(league, season)
            scraper.download()
            scraper.savecsv(filepath)
        except Exception as e:
            print(f"Errors: {e}")
            pass
#--------------------------------------------------------------------------------
