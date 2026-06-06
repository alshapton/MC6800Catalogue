import base64
import json
import time
import requests
from ebay_creds import ebay_creds
from rich.progress import Progress,SpinnerColumn, TextColumn


country="US"

if country=="US":
    ebay_marketplace="US"
    ebay_currency="USD"

if country=="GB":
    ebay_marketplace="GB"
    ebay_currency="GBP"

# Listing for ebay_creds.py if it ever gets lost !!!
#def ebay_creds():
#    CLIENT_ID="XXXXXXXXX"
#    CLIENT_SECRET="XXXXXXXXXX"
#    return CLIENT_ID,CLIENT_SECRET

CLIENT_ID,CLIENT_SECRET = ebay_creds()
LOCAL_JSON_FILE = "notpresent.json" 

if not CLIENT_ID or not CLIENT_SECRET:
    print("Error: Missing required eBay credentials in environment.")
    sys.exit(1)

print("Secrets successfully loaded! Proceeding with API authentication...")

def remove_duplicate_first_word(text):
    words = text.split()
    
    # Check if there are at least two words and if they are identical
    if len(words) > 1 and words[0] == words[1]:
        words.pop(1)  # Remove the duplicate second word
        
    return " ".join(words)

def get_ebay_application_token(client_id, client_secret):
    """Retrieves an application access token using Client Credentials grant flow"""
    url = "https://api.ebay.com/identity/v1/oauth2/token"
    
    # Base64 encode credentials as required by OAuth 2.0 standards
    credentials = f"{client_id}:{client_secret}"
    encoded_credentials = base64.b64encode(credentials.encode()).decode()
    
    headers = {
        "Content-Type": "application/x-www-form-urlencoded",
        "Authorization": f"Basic {encoded_credentials}"
    }
    payload = {
        "grant_type": "client_credentials",
        "scope": "https://api.ebay.com/oauth/api_scope"
    }
    
    response = requests.post(url, headers=headers, data=payload)
    if response.status_code == 200:
        return response.json().get("access_token")
    else:
        print(f"❌ OAuth failure: {response.status_code} - {response.text}")
        return None

def search_ebay_for_keyword(token, keyword):
    """Queries the eBay Browse API for a single keyword string"""
    url = "https://api.ebay.com/buy/browse/v1/item_summary/search"
    headers = {
        "Authorization": f"Bearer {token}",
        "X-EBAY-C-MARKETPLACE-ID": "EBAY_"+ebay_marketplace # Change to EBAY_GB if targeting the UK market
    }
    params = {
        "q": keyword,
        "limit": 5 # Limit to top 5 results per keyword to save bandwidth
    }
    
    response = requests.get(url, headers=headers, params=params)
    if response.status_code == 200:
        return response.json().get("itemSummaries", [])
    else:
        print(f"⚠️ Search failed for '{keyword}': Status {response.status_code}")
        return []

def main():
    # 1. Load keywords from local JSON tracking array
    try:
        with open(LOCAL_JSON_FILE, "r") as f:
            search_list = json.load(f)
    except FileNotFoundError:
        print(f"Error: Target file '{LOCAL_JSON_FILE}' not found.")
        return
        
    if not isinstance(search_list, list):
        print("Error: Input JSON must be structured as a flat list array.")
        return

    # 2. Authenticate cleanly with backend API endpoints
    print("Authenticating with eBay Developer API...")
    token = get_ebay_application_token(CLIENT_ID, CLIENT_SECRET)
    if not token:
        return
        
    print(f"Authentication verified. Processing {len(search_list)} source queries...\n")
    
    with Progress(
    SpinnerColumn(),  # Renders the animated spinner
    TextColumn("[progress.description]{task.description}"),  # Renders your text
                ) as progress:
        
        task_id = progress.add_task(description="[cyan]Initializing context...", total=None)    
    
        # 3. Iteratively execute the query parameters
        all_found_items = {}
        c = 0
        for keyword in search_list:
            sk=remove_duplicate_first_word(keyword["id"])
            
            c=c+1

            listings = search_ebay_for_keyword(token, sk)
            if listings:
                all_found_items[sk] = []
                for item in listings:
                    title = item.get("title")
                    price = item.get("price", {}).get("value")
                    currency = item.get("price", {}).get("currency", ebay_currency)
                    link = item.get("itemWebUrl")
                    
                    all_found_items[sk].append({
                        "title": title,
                        "price": f"{price} {currency}",
                        "link": link
                    })
            progress.update(task_id, description= str(c)+"/"+str(len(search_list)) + f" [yellow] {sk}.")            
            
            # Polite API cool-down pause to prevent accidental rate-limit throttling
            time.sleep(0.5)

        progress.update(task_id, description="[green]Task sequence complete!")
        progress.stop()

    # 4. Display mapped results to output file
    with open("newresults.txt", "w") as f:

        f.write("\n" + "="*40 + "\nCOMPLETED MARKET SEARCH REPORT\n" + "="*40)
        for search_term, items in all_found_items.items():
            f.write(f"\n📦 RESULTS FOR: '{search_term}' ({len(items)} found)\n")
            f.write("-" * 50)
            f.write("\n")
            for idx, match in enumerate(items, 1):
                f.write(f"  {idx}. {match['title']}\n")
                f.write(f"     Price: {match['price']}\n")
                f.write(f"     URL:   {match['link']}\n")

if __name__ == "__main__":
    main()