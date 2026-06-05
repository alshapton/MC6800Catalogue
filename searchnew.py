import base64
import json
import requests
import os
from ebay_creds import ebay_creds

ebay_marketplace="GB"
ebay_currency="GBP"

# Listing for ebay_creds.py if it ever gets lost !!!
#def ebay_creds():
#    CLIENT_ID="XXXXXXXXX"
#    CLIENT_SECRET="XXXXXXXXXX"
#    return CLIENT_ID,CLIENT_SECRET

CLIENT_ID,CLIENT_SECRET = ebay_creds()

if not CLIENT_ID or not CLIENT_SECRET:
    print("Error: Missing required eBay credentials in environment.")
    sys.exit(1)

print("Secrets successfully loaded! Proceeding with API authentication...")


LOCAL_JSON_FILE = "items.json" 

def get_ebay_token(CLIENT_ID, CLIENT_SECRET):
    """Obtains an official Application access token from eBay"""
    url = "https://api.ebay.com/identity/v1/oauth2/token"
    auth_credential = f"{CLIENT_ID}:{CLIENT_SECRET}"
    b64_credential = base64.b64encode(auth_credential.encode()).decode()
    
    headers = {
        "Content-Type": "application/x-www-form-urlencoded",
        "Authorization": f"Basic {b64_credential}"
    }
    payload = {
        "grant_type": "client_credentials",
        "scope": "https://api.ebay.com/oauth/api_scope"
    }
    
    response = requests.post(url, headers=headers, data=payload)
    if response.status_code == 200:
        return response.json().get("access_token")
    else:
        print(f"Authentication Failed: {response.status_code}\n{response.text}")
        return None

def fetch_ebay_listings(token, search_query):
    """Fetches up to 100 structured items from eBay's production servers"""
    url = "https://api.ebay.com/buy/browse/v1/item_summary/search"
    headers = {
        "Authorization": f"Bearer {token}",
        "X-EBAY-C-MARKETPLACE-ID": "EBAY_" + ebay_marketplace,
    }
    params = {
        "q": search_query,
        "limit": 200  # Pull a wide batch to catch manuals, terminals, etc.
    }
    
    response = requests.get(url, headers=headers, params=params)
    if response.status_code == 200:
        return response.json().get("itemSummaries", [])
    else:
        print(f"Search Request Failed: {response.status_code}\n{response.text}")
        return []

def cross_reference_listings():
    # Load your local MySQL JSON export
    try:
        with open(LOCAL_JSON_FILE, "r") as f:
            local_inventory = json.load(f)
    except FileNotFoundError:
        print(f"Error: Could not find local file '{LOCAL_JSON_FILE}'.")
        return

    # Create a lowercase list/set of your known part numbers or titles for easy matching
    # Adjust 'part_number' to match whatever key holds your data identifiers
    known_items = {str(item.get("name", "")).lower() for item in local_inventory if item.get("name")}

    # Authenticate and query eBay
    token = get_ebay_token(CLIENT_ID, CLIENT_SECRET)
    print(CLIENT_ID,CLIENT_SECRET)
    if not token:
        return

    # A broad query ensures the API pulls matching vintage ICs, manuals, and hardware
    search_term = "Motorola 6800"
    #search_term = search_term + "&filter=itemLocationCountry:GB"
    print(f"Querying eBay for '{search_term}' ecosystem...")
    ebay_items = fetch_ebay_listings(token, search_term)
    
    missing_from_local = []

    print("Comparing live market data against local inventory...\n")
    for item in ebay_items:
        title = item.get("title", "")
        title_lower = title.lower()
        price = item.get("price", {}).get("value", "0.00")
        currency = item.get("price", {}).get("currency", ebay_currency)
        link = item.get("itemWebUrl", "")

        # Check if the eBay item title contains ANY part number or keyword you already own
        # If your local DB stores whole titles, you could swap this for an exact lookup
        already_owned = any(known_item in title_lower for known_item in known_items)

        # ... inside the loop of your existing script ...
        if not already_owned:
            # Grab location
            loc = item.get("itemLocation", {})
            location_str = f"{loc.get('country', 'Unknown')}"
            
            missing_from_local.append({
                "title": title,
                "price": f"{price} {currency}",
                "location": location_str,
                "link": link
            })

    # Print out your final delta list
    if missing_from_local:
        print(f"Found {len(missing_from_local)} items on eBay NOT in your local database:")
        print("=" * 70)
        for idx, item in enumerate(missing_from_local, 1):
            print(f"{idx}. {item['title']}")
            print(f"   Location: {item['location']}")
            print(f"   Price:    {item['price']}")
            print(f"   URL:      {item['link']}\n")
    else:
        print("Everything found on eBay already exists in your local database file.")

if __name__ == "__main__":
    cross_reference_listings()