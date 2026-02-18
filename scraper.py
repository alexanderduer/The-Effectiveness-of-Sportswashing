import pandas as pd
from datetime import datetime
import praw
from praw.models import MoreComments


reddit = praw.Reddit(
    client_id="FJZ-ykkoBoO-BBKqYG-CPw",
    client_secret="Vxlq-kgxX9ONqtDsuN4Vj9hjSJzdOQ",
    user_agent="python:python scraping script:v1.0 (by /u/Significant-Win2911)"
)


subreddits = ["PoliticalDiscussion", "worldnews", "news", "europe", "france", "politics"]
keywords = ["France", "french", "Paris", "Frankreich", "français", "française", "französisch", "französische"]

start_timestamp = int(datetime(2023, 1, 1).timestamp())
end_timestamp = int(datetime(2025, 3, 1).timestamp())

data = []

for subreddit_name in subreddits:
    subreddit = reddit.subreddit(subreddit_name)

    query = "title:(" + " OR ".join(keywords) + ")"
    print(f"Searching in r/{subreddit_name} with query: {query}")


    posts = subreddit.search(query, limit=100000)

    post_count = 0

    for post in posts:
        print(f"[{subreddit_name}] Checking post: {post.title}")
        if start_timestamp <= post.created_utc < end_timestamp:
            post_count += 1
            post.comments.replace_more(limit=0)
            for comment in post.comments.list():
                if isinstance(comment, MoreComments):
                    continue
                data.append({
                    "Subreddit": subreddit_name,
                    "Post Title": post.title,
                    "Comment Author": comment.author if comment.author else "[deleted]",
                    "Comment Body": comment.body,
                    "Comment Date": datetime.utcfromtimestamp(comment.created_utc).strftime('%Y-%m-%d %H:%M:%S')
                })

    print(f"[{subreddit_name}] Total posts found: {post_count}")

df = pd.DataFrame(data)
#df.to_csv("reddit_before_after.csv", index=False, line_terminator='\n')

print("Scraping completed!")
