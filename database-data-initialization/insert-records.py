import sqlite3
import postgresql

def main():
    conn = sqlite3.connect('./db.sqlite3')
    c = conn.cursor()

    pos_conn = postgresql.open('pq://yesod-kumalan-devel:yesod-kumalan@localhost:5432/yesod-kumalan-devel')

    places = c.execute("SELECT * from place").fetchall()
    for place in places:
        p_ins = pos_conn.prepare("INSERT INTO place (name) VALUES ($1) returning id")
        place_id = p_ins(place[1])[0][0]
        
        users = c.execute("SELECT * from user where place_id == ?", (place[0],)).fetchall()
        for user in users:
            u_ins = pos_conn.prepare("INSERT INTO \"user\" (name, place_id, paid, exempted) VALUES ($1, $2, $3, $4) returning id")
            user_id = u_ins(user[4], place_id, user[2], user[3])[0][0]

            devices = c.execute("SELECT * from mac where user_id == ?", (user[0],)).fetchall()
            for device in devices:
                d_ins = pos_conn.prepare("INSERT INTO device (user_id, mac_address) VALUES ($1, $2)")
                d_ins(user_id, device[2])

if __name__ == "__main__":
    main()
